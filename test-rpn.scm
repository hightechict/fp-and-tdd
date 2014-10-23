;; Copyright 2012 Bas Bossink 
;; <bas.bossink@gmail.com>
;; http://basbossink.github.com/

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require-extension test)
(use numbers)
(include "mathh-constants")
(load "rpn.scm")

(test-begin "test-rpn")

(test 5 (rpn '(2 3 +)))
(test 6 (rpn '(2 3 *)))
(test 3 (rpn '(6 2 /)))
(test 1 (rpn '(3 2 -)))
(test 8 (rpn '(2 3 ^)))
(test 1 (rpn '(3 2 %)))
(test 2/3 (rpn '(2 3 /)))
(test 2+i (rpn '(2 0+i +)))
(test 0.0 (rpn '(0.0 sin)))
(test 2.0 (rpn '(1 0.0 cos +)))
(test 2.0 (rpn '(1 PI/2 sin +)))
(test 1.0 (rpn '(1 PI tan +)))

(test-end)
