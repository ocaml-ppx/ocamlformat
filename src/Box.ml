(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)
type t = {opn: Fmt.t; cls: Fmt.t}

let noop = {opn= Fmt.noop; cls= Fmt.noop}

let wrap {opn; cls} = Fmt.wrap_k opn cls

let unsafe_opn t = t.opn

let unsafe_cls t = t.cls

let unsafe_prepend_to_cls f t = {t with cls= f $ t.cls}

let compose ~inside ~outside =
  {opn= outside.opn $ inside.opn; cls= inside.cls $ outside.cls}

let vbox n = {opn= Fmt.open_vbox n; cls= Fmt.close_box}

let hvbox n = {opn= Fmt.open_hvbox n; cls= Fmt.close_box}

let hovbox n = {opn= Fmt.open_hovbox n; cls= Fmt.close_box}
