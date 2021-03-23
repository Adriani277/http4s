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

import cats.effect.Concurrent
import org.http4s.headers.`Content-Type`
import org.http4s.jawn.JawnInstances
import org.http4s.{EntityDecoder, EntityEncoder, MalformedMessageBodyFailure, MediaType}
import java.nio.charset.StandardCharsets
import zio.json._
import org.http4s.Message

trait ZIOJsonInstances {
  def jsonOf[F[_]: Concurrent, A: JsonDecoder]: EntityDecoder[F, A] =
    EntityDecoder.decodeBy[F, A](MediaType.application.json) { m =>
      EntityDecoder.collectBinary(m).subflatMap { chunk =>
        val str = new String(chunk.toArray, StandardCharsets.UTF_8)
        if (str.nonEmpty)
          str.fromJson.fold(e => Left(MalformedMessageBodyFailure(e, None)), Right(_))
        else
          Left(JawnInstances.defaultJawnEmptyBodyMessage)
      }
    }

  def jsonEncoderOf[F[_], A: JsonEncoder] = EntityEncoder
    .stringEncoder[F]
    .contramap[A](_.toJson)
    .withContentType(`Content-Type`(MediaType.application.json))

  implicit class MessageSyntax[F[_]: Concurrent](self: Message[F]) {
    def decodeJson[A: JsonDecoder]: F[A] =
      self.as(implicitly, jsonOf[F, A])
  }
}
