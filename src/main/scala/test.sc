case class CC(value: Int)

def fun(mod: CC => CC): CC

def mod(cc: CC)(implicit v: Int):CC = {
  cc.value match  {
    case `v` => cc
    case _ => CC(v)
  }
}

fun(mod)

