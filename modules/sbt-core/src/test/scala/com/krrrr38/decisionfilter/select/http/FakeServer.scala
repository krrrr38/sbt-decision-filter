package com.krrrr38.decisionfilter.select.http

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ArrayBuffer

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** A local HTTP server answering from a queue of canned responses, for backend tests. */
final class FakeServer(responses: Seq[(Int, Seq[(String, String)], String)]) extends AutoCloseable {
  import FakeServer.Request

  private val queue = scala.collection.mutable.Queue(responses: _*)
  val requests: ArrayBuffer[Request] = ArrayBuffer.empty

  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.createContext(
    "/",
    (ex: HttpExchange) => {
      val body = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
      val headers = scala.collection.mutable.Map.empty[String, String]
      ex.getRequestHeaders.forEach((k, vs) => headers.put(k.toLowerCase, vs.get(0)))
      val (status, respHeaders, respBody) = synchronized {
        requests += Request(ex.getRequestURI.getPath, headers.toMap, body)
        if (queue.isEmpty) (500, Nil, "no more canned responses") else queue.dequeue()
      }
      respHeaders.foreach { case (k, v) => ex.getResponseHeaders.add(k, v) }
      val bytes = respBody.getBytes(StandardCharsets.UTF_8)
      ex.sendResponseHeaders(status, bytes.length.toLong)
      ex.getResponseBody.write(bytes)
      ex.close()
    }
  )
  server.start()

  val url: String = s"http://127.0.0.1:${server.getAddress.getPort}"

  def close(): Unit = server.stop(0)
}

object FakeServer {
  final case class Request(path: String, headers: Map[String, String], body: String)
}
