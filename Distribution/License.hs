-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.License
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- The License datatype.  For more information about these and other
-- open-source licenses, you may visit <http://www.opensource.org/>.
--
-- I am not a lawyer, but as a general guideline, most Haskell
-- software seems to be released under a BSD3 license, which is very
-- open and free.  If you don't want to restrict the use of your
-- software or its source code, use BSD3 or PublicDomain.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.License (
	License(..)
  ) where

-- |This datatype indicates the license under which your package is
-- released.  It is also wise to add your license to each source file.
-- The 'AllRightsReserved' constructor is not actually a license, but
-- states that you are not giving anyone else a license to use or
-- distribute your work.  The comments below are general guidelines.
-- Please read the licenses themselves and consult a lawyer if you are
-- unsure of your rights to release the software.

data License = GPL  -- ^GNU Public License. Source code must accompany alterations.
             | LGPL -- ^Lesser GPL, Less restrictive than GPL, useful for libraries.
             | BSD3 -- ^3-clause BSD license, newer, no advertising clause. Very free license.
             | BSD4 -- ^4-clause BSD license, older, with advertising clause.
             | PublicDomain -- ^Holder makes no claim to ownership, least restrictive license.
             | AllRightsReserved -- ^No rights are granted to others. Undistributable. Most restrictive.
             | {- ... | -} OtherLicense -- ^Some other license.
               deriving (Read, Show, Eq)
