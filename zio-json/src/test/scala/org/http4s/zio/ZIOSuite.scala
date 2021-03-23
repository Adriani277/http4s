/*
 * Copyright 2018 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.zio

import org.http4s.jawn.JawnDecodeSupportSuite
import zio.json.ast.Json

import cats.effect.IO
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType
import org.http4s.jawn.JawnDecodeSupportSuite
import zio.json.DeriveJsonCodec
import org.http4s.Request
import org.http4s.MalformedMessageBodyFailure

class ZIOSuite extends JawnDecodeSupportSuite[Json] {
  private val jsonDecoder = jsonOf[IO, Json]
  testJsonDecoder(jsonDecoder)
  testJsonDecoderError(jsonDecoder)(
    emptyBody = { case MalformedMessageBodyFailure("Custom Invalid JSON: empty body", _) => true },
    parseError = { case MalformedMessageBodyFailure("Custom Invalid JSON circe", _) => true }
  )

  sealed case class Foo(bar: String)
  object Foo {
    implicit val codec = DeriveJsonCodec.gen[Foo]
  }
  val foo = Foo("hello")

  test("jsonEncoderOf should have json content type") {
    val ct: Option[`Content-Type`] = jsonEncoderOf[IO, Json].headers.get[`Content-Type`]
    assertEquals(ct, Some(`Content-Type`(MediaType.application.json)))
  }

  test("jsonEncoderOf should write compact JSON") {
    implicit val encoder = jsonEncoderOf[IO, Foo]
    writeToString(Foo("ZIOSupport")).assertEquals("""{"bar":"ZIOSupport"}""")
  }

  test("jsonOf should decode JSON from a zio-json decoder") {
    val result = jsonOf[IO, Foo]
      .decode(
        Request[IO]()
          .withEntity("""{"bar":"decode me"}""")
          .withContentType(`Content-Type`(MediaType.application.json)),
        strict = true)
    result.value.assertEquals(Right(Foo("decode me")))
  }

  test("Message[F].decodeJson[A] should decode json from a message") {
    val req = Request[IO]()
      .withEntity("""{"bar":"decode me from message"}""")
      .withContentType(`Content-Type`(MediaType.application.json))
    req.decodeJson[Foo].assertEquals(Foo("decode me from message"))
  }

  test("Message[F].decodeJson[A] should fail on invalid json") {
    val req = Request[IO]()
      .withEntity("""{"bad":"json"}""")
      .withContentType(`Content-Type`(MediaType.application.json))
    req.decodeJson[Foo].attempt.map(_.isLeft).assert
  }

  test("ZIOEntityCodec should encode without defining EntityEncoder using default printer") {
    import org.http4s.zio.ZIOEntityCodec._
    writeToString(foo).assertEquals("""{"bar":"hello"}""")
  }
}
