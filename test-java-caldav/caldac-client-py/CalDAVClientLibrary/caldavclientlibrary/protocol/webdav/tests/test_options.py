##
# Copyright (c) 2007-2013 Apple Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
##

from caldavclientlibrary.protocol.http.session import Session
from caldavclientlibrary.protocol.webdav.options import Options

import unittest

class TestRequest(unittest.TestCase):

    def test_Method(self):

        server = Session("www.example.com")
        request = Options(server, "/")
        self.assertEqual(request.getMethod(), "OPTIONS")



class TestRequestHeaders(unittest.TestCase):
    pass



class TestRequestBody(unittest.TestCase):
    pass



class TestResponse(unittest.TestCase):
    pass



class TestResponseHeaders(unittest.TestCase):

    def test_OneHeader(self):

        server = Session("www.example.com")
        request = Options(server, "/")
        request.getResponseHeaders().update({
            "allow": ("GET, PUT, OPTIONS, HEAD",),
        })
        self.assertEqual(set(request.getAllowed()), set(("GET", "PUT", "OPTIONS", "HEAD")))
        self.assertTrue(request.isAllowed("GET"))
        self.assertTrue(request.isAllowed("PUT"))
        self.assertTrue(request.isAllowed("OPTIONS"))
        self.assertTrue(request.isAllowed("HEAD"))


    def test_MultipleHeader(self):

        server = Session("www.example.com")
        request = Options(server, "/")
        request.getResponseHeaders().update({
            "allow": ("GET, PUT", "OPTIONS, HEAD",),
        })
        self.assertEqual(set(request.getAllowed()), set(("GET", "PUT", "OPTIONS", "HEAD")))
        self.assertTrue(request.isAllowed("GET"))
        self.assertTrue(request.isAllowed("PUT"))
        self.assertTrue(request.isAllowed("OPTIONS"))
        self.assertTrue(request.isAllowed("HEAD"))



class TestResponseBody(unittest.TestCase):
    pass
