/*
 *  Copyright 2021-2022 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s.weavertests

import weaver._
import smithy4s.example._
import smithy4s.http4s._
import cats.effect.IO

object WeaverComplianceTest extends SimpleIOSuite {
  implicit val compatEffect: CompatEffect = new CompatEffect
  implicit val asserter = new Asserter[IO]() {}
  val testGenerator = new ClientHttpComplianceTestCase(
    smithy4s.api.SimpleRestJson(),
    SimpleRestJsonBuilder(HelloServiceGen).client[IO]
  )
  val tests: List[ComplianceTest[IO]] =
    testGenerator.allClientTests(testGenerator.forApp())

  tests.foreach(tc =>
    test(tc.name) {
      tc.run.map[Expectations] {
        case Left(value) =>
          Expectations.Helpers.failure(value)
        case Right(_) =>
          Expectations.Helpers.success
      }
    }
  )
}
