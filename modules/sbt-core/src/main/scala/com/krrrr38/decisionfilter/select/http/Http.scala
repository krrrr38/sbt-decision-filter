package com.krrrr38.decisionfilter.select.http

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

import com.krrrr38.decisionfilter.select.BackendError

final case class HttpResult(status: Int, body: String, headers: Map[String, String]) {
  def header(name: String): Option[String] = headers.get(name.toLowerCase(java.util.Locale.ROOT))
}

/** What a backend makes of a response. */
sealed abstract class Outcome extends Product with Serializable
object Outcome {
  case object Success extends Outcome
  case object RateLimited extends Outcome
  case object Transient extends Outcome
  final case class Fatal(error: BackendError) extends Outcome
}

/** The JDK HTTP client plus the retry policy shared by every backend (spec 6.7):
  *   - network errors and 5xx: up to `retries` more attempts, exponential backoff
  *   - 429: wait as `Retry-After` says (or a short jittered wait), up to `rateLimitRetries` times
  *   - everything else is decided by the backend's `classify`
  */
final class Http(
    requestTimeout: Duration = Duration.ofSeconds(120),
    retries: Int = 3,
    rateLimitRetries: Int = 8,
    sleep: Long => Unit = ms => Thread.sleep(ms),
    send: Http.Send = Http.jdkSend(HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(20)).build())
) {

  def postJson(
      url: String,
      headers: Seq[(String, String)],
      body: String,
      classify: HttpResult => Outcome
  ): Either[BackendError, HttpResult] = {
    var attempt = 0
    var limited = 0
    var result: Option[Either[BackendError, HttpResult]] = None
    while (result.isEmpty) {
      send(url, headers, body, requestTimeout) match {
        case Left(e) =>
          if (attempt >= retries) result = Some(Left(BackendError.Failed(s"network: ${trim(e.toString)}")))
          else {
            sleep(Http.backoffMs(attempt, None))
            attempt += 1
          }
        case Right(res) =>
          classify(res) match {
            case Outcome.Success => result = Some(Right(res))
            case Outcome.Fatal(err) => result = Some(Left(err))
            case Outcome.RateLimited =>
              if (limited >= rateLimitRetries)
                result = Some(Left(BackendError.Failed(s"HTTP ${res.status}: rate limited ${limited + 1} times")))
              else {
                limited += 1
                sleep(Http.rateLimitWaitMs(limited, Http.retryAfterMs(res)))
              }
            case Outcome.Transient =>
              if (attempt >= retries) result = Some(Left(BackendError.Failed(s"HTTP ${res.status}: ${trim(res.body)}")))
              else {
                sleep(Http.backoffMs(attempt, Http.retryAfterMs(res)))
                attempt += 1
              }
          }
      }
    }
    result.get
  }

  private def trim(s: String): String = {
    val one = s.replaceAll("\\s+", " ").trim
    if (one.length > 300) one.substring(0, 300) + "..." else one
  }
}

object Http {

  type Send = (String, Seq[(String, String)], String, Duration) => Either[Throwable, HttpResult]

  def jdkSend(client: HttpClient): Send = (url, headers, body, timeout) =>
    try {
      val b = HttpRequest
        .newBuilder(URI.create(url))
        .timeout(timeout)
        .header("content-type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body))
      headers.foreach { case (k, v) => b.header(k, v) }
      val res = client.send(b.build(), HttpResponse.BodyHandlers.ofString())
      val hs = scala.collection.mutable.Map.empty[String, String]
      res.headers().map().forEach { (k, vs) =>
        if (!vs.isEmpty) hs.put(k.toLowerCase(java.util.Locale.ROOT), vs.get(0))
      }
      Right(HttpResult(res.statusCode(), res.body(), hs.toMap))
    } catch {
      case e: InterruptedException => Thread.currentThread().interrupt(); Left(e)
      case e: Exception            => Left(e)
    }

  /** `Retry-After` in seconds, or OpenAI's `retry-after-ms`. */
  def retryAfterMs(res: HttpResult): Option[Long] = {
    def num(s: String): Option[Double] =
      try Some(s.trim.toDouble).filter(d => !d.isNaN && d >= 0)
      catch { case _: NumberFormatException => None }
    res.header("retry-after-ms").flatMap(num).map(_.toLong)
      .orElse(res.header("retry-after").flatMap(num).map(s => (s * 1000).toLong))
      .map(ms => math.min(ms, 60000L))
  }

  def backoffMs(attempt: Int, hinted: Option[Long]): Long =
    hinted.getOrElse((math.min(20000.0, 500.0 * math.pow(2, attempt.toDouble)) * jitter()).toLong)

  def rateLimitWaitMs(nth: Int, hinted: Option[Long]): Long =
    hinted.getOrElse((math.min(5000.0, 300.0 * math.pow(1.5, (nth - 1).toDouble)) * jitter()).toLong)

  private def jitter(): Double = 0.5 + java.util.concurrent.ThreadLocalRandom.current().nextDouble()
}
