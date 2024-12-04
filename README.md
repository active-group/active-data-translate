[![Clojars Project](https://img.shields.io/clojars/v/de.active-group/active-data-translate.svg)](https://clojars.org/de.active-group/active-data-translate)
[![cljdoc badge](https://cljdoc.org/badge/de.active-group/active-data-translate)](https://cljdoc.org/d/de.active-group/active-data-translate/CURRENT)

This goal of this library is to facilitate translating back and forth
between data in a form described by [[realms]] and a different form,
usually EDN or Transit.

[Latest Version](https://clojars.org/de.active-group/active-data-translate)

[API Docs](https://cljdoc.org/d/de.active-group/active-data-translate/CURRENT)

## Introduction

Given some data described by realms, for example a record:

```clojure
(require '[active.data.record :as r])
(require '[active.data.realm :as realm])

(r/def-record user
  [user-id realm/integer
   user-name realm/string])
```

We can define a format that translates between instances of the record
an EDN compatible map representation of the record, and identity
translations for strings and integers:

```clojure
(require '[active.data.translate.format :as format])
(require '[active.data.translate.formatter :as formatter])

(def my-edn-format
  (format :my-edn-format
          {realm/string formatter/id
           realm/integer formatter/id
           user (formatter/record-map user {user-id :id
                                            user-name :name})}))
```

Using the format, we can create functions that translate between the
two:

```clojure
(require '[active.data.translate.core :as translate])

(def edn-from-user (translate/translator-from user my-edn-format))
(def edn-to-user (translate/translator-to user my-edn-format))

(= {:id 1 :name "Alice"}
   (edn-from-user (user user-id 1 user-name "Alice"))

(= (user user-id 1 user-name "Alice")
   (edn-to-user {:id 1 :name "Alice"})
```

## License

Copyright © 2024 Active Group GmbH

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
