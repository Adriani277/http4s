/*
 * Copyright 2013 http4s.org
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

package org.http4s.parser

import org.http4s.Uri.Scheme.https
import org.http4s._
import org.http4s.Uri._
import org.typelevel.ci.CIString

class UriParserSpec extends Http4sSpec {
  "Uri.requestTarget" should {
    def check(items: Seq[(String, Uri)]) =
      foreach(items) { case (str, uri) =>
        Uri.requestTarget(str) must beRight(uri)
      }

    // RFC 3986 examples
    // http://tools.ietf.org/html/rfc3986#section-1.1.2

    // http://www.ietf.org/rfc/rfc2396.txt

    "parse a IPv6 address" in {
      val v = "01ab:01ab:01ab:01ab:01ab:01ab:01ab:01ab" +: (for {
        h <- 0 to 7
        l <- 0 to 7 - h
        f = List.fill(h)("01ab").mkString(":")
        b = List.fill(l)("32ba").mkString(":")
        if (f ++ b).size < 7 // a single shortened section is disallowed
      } yield f + "::" + b)

      foreach(v) { s =>
        Uri.Parser.ipv6Address.string.parseAll(s) must beRight(s)
      }
    }

    "parse a IPv4 address" in {
      foreach(0 to 255) { i =>
        val addr = s"$i.$i.$i.$i"
        Ipv4Address.fromString(addr).map(_.value) must beRight(addr)
      }
    }

    "parse a short IPv6 address in brackets" in {
      val s = "[01ab::32ba:32ba]"
      Uri.requestTarget(s) must beRight(
        Uri(authority = Some(Authority(host = ipv6"01ab::32ba:32ba"))))
    }

    "handle port configurations" in {
      val portExamples: Seq[(String, Uri)] = Seq(
        (
          "http://foo.com",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = RegName(CIString("foo.com")), port = None)))),
        (
          "http://foo.com:",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = RegName(CIString("foo.com")), port = None)))),
        (
          "http://foo.com:80",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = RegName(CIString("foo.com")), port = Some(80)))))
      )

      check(portExamples)
    }

    "parse absolute URIs" in {
      val absoluteUris: Seq[(String, Uri)] = Seq(
        (
          "http://www.foo.com",
          Uri(Some(Scheme.http), Some(Authority(host = RegName(CIString("www.foo.com")))))),
        (
          "http://www.foo.com/foo?bar=baz",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = RegName(CIString("www.foo.com")))),
            path"/foo",
            Query.fromPairs("bar" -> "baz"))),
        ("http://192.168.1.1", Uri(Some(Scheme.http), Some(Authority(host = ipv4"192.168.1.1")))),
        (
          "http://192.168.1.1:80/c?GB=object&Class=one",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = ipv4"192.168.1.1", port = Some(80))),
            path"/c",
            Query.fromPairs("GB" -> "object", "Class" -> "one"))),
        (
          "http://[2001:db8::7]/c?GB=object&Class=one",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = ipv6"2001:db8::7")),
            path"/c",
            Query.fromPairs("GB" -> "object", "Class" -> "one"))),
        (
          "mailto:John.Doe@example.com",
          Uri(Some(scheme"mailto"), path = Uri.Path.fromString("John.Doe@example.com")))
      )

      check(absoluteUris)
    }

    "parse relative URIs" in {
      val relativeUris: Seq[(String, Uri)] = Seq(
        ("/foo/bar", Uri(path = path"/foo/bar")),
        (
          "/foo/bar?foo=bar&ding=dong",
          Uri(path = path"/foo/bar", query = Query.fromPairs("foo" -> "bar", "ding" -> "dong"))),
        ("/", Uri(path = Uri.Path.Root))
      )

      check(relativeUris)
    }

    "parse relative URI with empty query string" in {
      val u = Uri.requestTarget("/foo/bar?")
      u must beRight(Uri(path = path"/foo/bar", query = Query("" -> None)))
    }

    {
      val q = Query.fromString("param1=3&param2=2&param2=foo")
      val u = Uri(query = q)
      "represent query as multiParams as a Map[String,Seq[String]]" in {
        u.multiParams must be_==(Map("param1" -> Seq("3"), "param2" -> Seq("2", "foo")))
      }

      "parse query and represent params as a Map[String,String] taking the first param" in {
        u.params must be_==(Map("param1" -> "3", "param2" -> "2"))
      }
    }

    "fail on invalid uri" in {
      val invalid = Seq("^", "]", "/hello/wo%2rld", "/hello/world?bad=enc%ode")
      forall(invalid) { _ =>
        Uri.fromString("^") must beLeft
        Uri.requestTarget("^") must beLeft
      }
    }
  }

  "Uri.fromString" should {
    def check(items: Seq[(String, Uri)]) =
      foreach(items) { case (str, uri) =>
        Uri.fromString(str) must beRight(uri)
      }

    "parse absolute URIs" in {
      val absoluteUris: Seq[(String, Uri)] = Seq(
        (
          "http://www.foo.com",
          Uri(Some(Scheme.http), Some(Authority(host = RegName(CIString("www.foo.com")))))),
        (
          "http://www.foo.com/foo?bar=baz",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = RegName(CIString("www.foo.com")))),
            path"/foo",
            Query.fromPairs("bar" -> "baz"))),
        ("http://192.168.1.1", Uri(Some(Scheme.http), Some(Authority(host = ipv4"192.168.1.1")))),
        (
          "http://192.168.1.1:80/c?GB=object&Class=one",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = ipv4"192.168.1.1", port = Some(80))),
            path"/c",
            Query.fromPairs("GB" -> "object", "Class" -> "one"))),
        (
          "http://[2001:db8::7]/c?GB=object&Class=one",
          Uri(
            Some(Scheme.http),
            Some(Authority(host = ipv6"2001:db8::7")),
            path"/c",
            Query.fromPairs("GB" -> "object", "Class" -> "one"))),
        (
          "mailto:John.Doe@example.com",
          Uri(Some(scheme"mailto"), path = Uri.Path.fromString("John.Doe@example.com")))
      )

      check(absoluteUris)
    }

    "parse a path-noscheme uri" in {
      Uri.fromString("q") must beRight.like { case u =>
        u must_== Uri(path = path"q")
      }
      Uri.fromString("a/b") must beRight.like { case u =>
        u must_== Uri(path = path"a/b")
      }
    }

    "parse a path-noscheme uri with query" in {
      Uri.fromString("a/b?foo") must beRight.like { case u =>
        u must_== Uri(path = path"a/b", query = Query(("foo", None)))
      }
    }

    "parse a path-absolute uri" in {
      Uri.fromString("/a/b") must beRight.like { case u =>
        u must_== Uri(path = path"/a/b")
      }
    }
    "parse a path-absolute uri with query" in {
      Uri.fromString("/a/b?foo") must beRight.like { case u =>
        u must_== Uri(path = path"/a/b", query = Query(("foo", None)))
      }
    }
    "parse a path-absolute uri with query and fragment" in {
      Uri.fromString("/a/b?foo#bar") must beRight.like { case u =>
        u must_== Uri(path = path"/a/b", query = Query(("foo", None)), fragment = Some("bar"))
      }
    }
  }

  "String interpolator" should {
    "parse valid URIs" in {
      uri"https://http4s.org" must_== Uri(
        scheme = Option(https),
        authority = Option(Uri.Authority(host = RegName(CIString("http4s.org")))))
    }

    "reject invalid URIs" in {
      illTyped {
        """
           uri"not valid"
        """
      }
      true
    }
  }
}
