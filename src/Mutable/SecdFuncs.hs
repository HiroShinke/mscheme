

module Mutable.SecdFuncs where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Error
import Mutable.SExpr
import Mutable.Secd
import qualified Primitives.Primitives as P


-- リスト操作
car :: SecdFunc
car _  = transPrim P.car

cdr :: SecdFunc
cdr _  = transPrim P.cdr

cons :: SecdFunc
cons _ = transPrim P.cons

pair :: SecdFunc
pair _  = transPrim P.pair

adds :: SecdFunc
adds _  = transPrim P.adds

subs :: SecdFunc
subs _  = transPrim P.subs

muls :: SecdFunc
muls _  = transPrim P.muls
  
divs :: SecdFunc
divs _  = transPrim P.divs

mod' :: SecdFunc
mod' _ = transPrim P.mod'

-- 等値の判定
eq' :: SecdFunc
eq' _ = transPrim P.eq'

equal' :: SecdFunc
equal' _ e = (transPrim P.equal') e

eqNum, ltNum, gtNum, ltEq, gtEq :: SecdFunc
eqNum _ = transPrim P.eqNum
ltNum _ = transPrim P.ltNum
gtNum _ = transPrim P.gtNum
ltEq  _ = transPrim P.ltEq
gtEq  _ = transPrim P.gtEq

  -- list

list' :: SecdFunc
list' _  = transPrim P.list'

-- error

error' :: SecdFunc
error' _  = transPrim P.error'

-- append
append' :: SecdFunc
append' _  = transPrim P.append'

