(*
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

(*
 * A URI.
 *)
type uri =
   { uri_proto : string;
     uri_host : string;
     uri_port : int option;
     uri_path : string
   }

(*
 * Byte ranges.
 *)
type byte_range =
   { byte_range_start : int;
     byte_range_end : int;
     byte_range_total : int
   }

(*
 * Content types.
 *)
type text_type =
   TextTypePlain
 | TextTypeHTML

type image_type =
   ImageTypeJPEG
 | ImageTypeGIF

type video_type =
   VideoTypeQuicktime
 | VideoTypeAVI

type audio_type =
   AudioTypeBasic
 | AudioTypeOgg
 | AudioTypeMpegUrl

type content_type =
   { content_type_main : string;
     content_type_sub  : string;
     content_type_params : (string * string) list
   }

type content_disposition =
   { content_disposition_type : string;
     content_disposition_params : (string * string) list
   }

(*
 * Request headers.
 *)
type cache_request_directive =
   CacheRequestNoCache
 | CacheRequestNoStore
 | CacheRequestMaxAge of int
 | CacheRequestMaxStale of int
 | CacheRequestMinFresh of int
 | CacheRequestNoTransform
 | CacheRequestOnlyIfCached
 | CacheRequestExtension0 of string
 | CacheRequestExtension1 of string
 | CacheRequestExtension2 of string * string

type request_header_entry =
   RequestAccept of string
 | RequestAcceptCharset of string
 | RequestAcceptEncoding of string
 | RequestAcceptLanguage of string
 | RequestAcceptRanges of string
 | RequestAge of int
 | RequestAllow of string list
 | RequestAuthorization of string
 | RequestCacheControl of cache_request_directive
 | RequestConnection of string
 | RequestContentType of content_type
 | RequestContentLength of int
 | RequestContentDisposition of content_disposition
 | RequestCookies of (string * string) list
 | RequestDate of float
 | RequestExpect
 | RequestFrom of string
 | RequestHost of string * int option
 | RequestIfMatch of string list option
 | RequestIfModifiedSince of float
 | RequestIfNoneMatch of string
 | RequestIfRange of string
 | RequestIfUnmodifiedSince of float
 | RequestMaxForwards of int
 | RequestPragma of string
 | RequestProxyAuthorization of string
 | RequestRange of byte_range
 | RequestReferer of uri
 | RequestTE of string
 | RequestTrailer of string
 | RequestTransferEncoding
 | RequestUpgrade of string list
 | RequestUserAgent of string
 | RequestVia of string
 | RequestWarning of string
 | RequestExtension of string * string

type request_header = request_header_entry list

(*
 * Post request body.
 *)
type request_post = (string * string) list

(*
 * Return code.
 *)
type response_code =
   ContinueCode
 | OkCode
 | CreatedCode
 | AcceptedCode
 | NoContentCode
 | MovedPermCode
 | MovedTempCode
 | SeeOtherCode
 | NotModifiedCode
 | BadRequestCode
 | UnauthorizedCode
 | ForbiddenCode
 | NotFoundCode
 | ServerErrorCode
 | NotImplementedCode
 | BadGatewayCode
 | ServiceUnavailableCode

(*
 * Response headers.
 *)
type cache_response_directive =
   CacheResponsePublic
 | CacheResponsePrivate
 | CacheResponseNoCache of string option
 | CacheResponseNoStore
 | CacheResponseNoTransform
 | CacheResponseMustRevalidate
 | CacheResponseProxyRevalidate
 | CacheResponseMaxAge of int
 | CacheResponseSMaxAge of int
 | CacheResponseExtension1 of string
 | CacheResponseExtension2 of string * string

type response_header_entry =
   ResponseCacheControl of cache_response_directive
 | ResponseConnect of string
 | ResponseContentEncoding of string
 | ResponseContentLanguage of string
 | ResponseContentLength of int
 | ResponseContentLocation of string
 | ResponseContentMD5 of string
 | ResponseContentRange of int * int * int
 | ResponseContentType of content_type
 | ResponseDate of float
 | ResponseETag of string
 | ResponseExpect of string
 | ResponseExpires of float
 | ResponseLastModified of float
 | ResponseLocation of string
 | ResponseProxyAuthenticate of string
 | ResponseRange of byte_range
 | ResponseRetryAfter of float
 | ResponseServer of string
 | ResponseTrailer of string
 | ResponseTransferEncoding of string
 | ResponseUpgrade of string list
 | ResponseVary of string list
 | ResponseVia of string
 | ResponseWarning of string
 | ResponseWWWAuthenticate of string

type response_header = response_header_entry list

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
