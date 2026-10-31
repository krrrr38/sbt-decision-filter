package com.krrrr38.decisionfilter.select.http

import scala.collection.mutable.ArrayBuffer

import com.krrrr38.decisionfilter.select.BackendError
import org.scalatest.funsuite.AnyFunSuite

class HttpSpec extends AnyFunSuite {

  private def http(responses: Either[Throwable, HttpResult]*): (Http, ArrayBuffer[Long]) = {
    val queue = scala.collection.mutable.Queue(responses: _*)
    val sleeps = ArrayBuffer.empty[Long]
    val h = new Http(sleep = ms => sleeps += ms, send = (_, _, _, _) => queue.dequeue())
    (h, sleeps)
  }

  private def res(status: Int, headers: (String, String)*) = Right(HttpResult(status, s"body $status", headers.toMap))

  private val classify: HttpResult => Outcome = r =>
    if (r.status == 200) Outcome.Success
    else if (r.status == 429) Outcome.RateLimited
    else if (r.status >= 500) Outcome.Transient
    else Outcome.Fatal(BackendError.Failed(s"HTTP ${r.status}"))

  private def post(h: Http) = h.postJson("http://x", Nil, "{}", classify)

  test("5xx and network errors are retried up to three times") {
    val (h, sleeps) = http(res(503), Left(new java.io.IOException("reset")), res(500), res(200))
    assert(post(h).map(_.status) == Right(200))
    assert(sleeps.size == 3)

    val (h2, _) = http(res(503), res(503), res(503), res(503))
    assert(post(h2) == Left(BackendError.Failed("HTTP 503: body 503")))
  }

  test("429 waits as Retry-After says") {
    val (h, sleeps) = http(res(429, "retry-after" -> "2"), res(429, "retry-after-ms" -> "150"), res(200))
    assert(post(h).map(_.status) == Right(200))
    assert(sleeps == ArrayBuffer(2000L, 150L))
  }

  test("other statuses are not retried") {
    val (h, sleeps) = http(res(404), res(200))
    assert(post(h) == Left(BackendError.Failed("HTTP 404")))
    assert(sleeps.isEmpty)
  }
}
