package functions

import java.net.URL
import java.nio.charset.StandardCharsets
import java.util.Base64

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}
import io.circe.Decoder.Result
import io.circe.parser._
import io.circe.{Decoder, HCursor, Json}
import org.fusesource.scalate.{TemplateEngine, TemplateSource}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining._

class NowPlayingFunction extends HttpFunction {

  val RefreshTokenEndpoint: String     = "https://accounts.spotify.com/api/token"
  val CurrentlyPlayingEndpoint: String = "https://api.spotify.com/v1/me/player/currently-playing?market=from_token"
  val RecentlyPlayedEndpoint: String   = "https://api.spotify.com/v1/me/player/recently-played"
  val RecentlyPlayedTrackLimit: Int    = 5

  val RefreshToken                     =  System.getenv("REFRESH_TOKEN")
  val ClientId: String                 =  System.getenv("CLIENT_ID")
  val SecretId: String                 =  System.getenv("SECRET_ID")

  val random          = scala.util.Random
  val engine          = new TemplateEngine()
  engine.escapeMarkup = false

  val TemplateName: String  = "spotify.html.ssp"
  val ContentBarDiv: String = "<div class='bar'></div>"
  val ContentBarCSS: String = ".bar:nth-child(%s)  { left: %spx; animation-duration: %sms; }"
  val BarsNumber: Int       = 84

  override def service(httpRequest: HttpRequest, httpResponse: HttpResponse): Unit =
    getAccessToken()
      .pipe(accessToken =>
        getCurrentlyPlaying(accessToken)
          .getOrElse(getRecentlyPlayed(accessToken))
          .pipe(renderTemplate)
          .tap { renderedTemplate =>
            httpResponse.setContentType("image/svg+xml; charset=utf-8")
            httpResponse.getWriter.write(renderedTemplate)
          }
      )

  private def getAccessToken(): String =
    Base64.getEncoder.encodeToString(s"$ClientId:$SecretId".getBytes(StandardCharsets.UTF_8))
      .pipe(encodedClientSecretId =>
        requests.post(
          RefreshTokenEndpoint,
          data = Map("grant_type" -> "refresh_token", "refresh_token" -> RefreshToken),
          headers = Map("Authorization" -> s"Basic $encodedClientSecretId")
        ).text()
      )
      .pipe(
        parse(_)
          .getOrElse(Json.Null)
          .hcursor
          .get[String]("access_token")
          .getOrElse("")
      )

  private def getCurrentlyPlaying(accessToken: String): Option[CurrentlyPlayingTrack] = {
    import CurrentlyPlayingTrack._

    requests.get(
      CurrentlyPlayingEndpoint,
      headers = Map("Authorization" -> s"Bearer $accessToken")
    )
      .pipe(response => Option.when(response.statusCode == 200)(response))
      .flatMap(response =>
        parse(response.text)
          .flatMap(_.as[CurrentlyPlayingTrack])
          .toOption
      )
  }

  private def getRecentlyPlayed(accessToken: String): RecentlyPlayedTrack = {
    import RecentlyPlayedTrack._

    requests.get(
      RecentlyPlayedEndpoint,
      params = Map("limit" -> RecentlyPlayedTrackLimit.toString),
      headers = Map("Authorization" -> s"Bearer $accessToken")
    ).text()
      .pipe(
        parse(_)
          .flatMap(_.as[RecentlyPlayedTrack])
          .getOrElse(RecentlyPlayedTrack("", "", "", ""))
      )
  }

  private def renderTemplate(currentlyPlaying: Track): String =
    Source.fromResource(TemplateName)
      .pipe(TemplateSource.fromSource(TemplateName, _))
      .pipe { templateSource =>
        val attributes = Map(
          "songName"   -> currentlyPlaying.song,
          "artistName" -> currentlyPlaying.artist,
          "contentBar" -> contentBar(),
          "image"      -> encodeImage(currentlyPlaying),
          "barCSS"     -> barCSS()
        )
        engine.layout(templateSource, attributes)
      }

  private def contentBar(): String =
    List.fill(BarsNumber)(ContentBarDiv).mkString("")

  private def encodeImage(currentlyPlaying: Track): String =
    new URL(currentlyPlaying.image)
      .openStream()
      .pipe(inputStream => Base64.getEncoder.encodeToString(inputStream.readAllBytes()))

  private def barCSS(): String = {
    @tailrec
    def loop(left: Int, list: List[String], index: Int): List[String] = {
      if (index <= BarsNumber) {
        val anim = random.between(1000, 1350 + 1)
        loop(left + 4, list :+ ContentBarCSS.format(index, left, anim), index + 1)
      } else {
        list
      }
    }
    loop(1, List.empty[String], 1).mkString("\n")
  }

  sealed trait Track {
    val artist: String
    val song: String
    val status: String
    val image: String
  }
  case class CurrentlyPlayingTrack(artist: String, song: String, status: String, image: String) extends Track
  case class RecentlyPlayedTrack(artist: String, song: String, status: String, image: String) extends Track

  object CurrentlyPlayingTrack {

    implicit val decoderCurrentlyPlayingTrack: Decoder[CurrentlyPlayingTrack] = new Decoder[CurrentlyPlayingTrack] {
      override def apply(c: HCursor): Result[CurrentlyPlayingTrack] =
        for {
          artists <- c.downField("item").downField("artists").as[List[Json]]
          artist  <- artists.head.hcursor.downField("name").as[String]
          song    <- c.downField("item").downField("name").as[String]
          images  <- c.downField("item").downField("album").downField("images").as[List[Json]]
          image   <- images(1).hcursor.downField("url").as[String]
        } yield CurrentlyPlayingTrack(artist = artist, song = song, status = "", image = image)
    }
  }

  object RecentlyPlayedTrack {

    implicit val decoderRecentlyPlayedTrack: Decoder[RecentlyPlayedTrack] = new Decoder[RecentlyPlayedTrack] {
      override def apply(c: HCursor): Result[RecentlyPlayedTrack] = {
        val x = random.between(0, RecentlyPlayedTrackLimit)
        for {
          tracks  <- c.downField("items").as[List[Json]]
          track   <- tracks(x).hcursor.downField("track").as[Json]
          artists <- track.hcursor.downField("artists").as[List[Json]]
          artist  <- artists.head.hcursor.downField("name").as[String]
          song    <- track.hcursor.downField("name").as[String]
          images  <- track.hcursor.downField("album").downField("images").as[List[Json]]
          image   <- images(1).hcursor.downField("url").as[String]
        } yield RecentlyPlayedTrack(artist = artist, song = song, status = "", image = image)
      }
    }
  }
}
