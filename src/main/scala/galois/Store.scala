package galois

sealed trait Store {
  def alpha: AStore
  def gamma: CStore
}

class CStore() extends Store {
  var cStore = Map[Address, Closure]()

  def extend(a: Address, e: Closure): Address = {
    cStore += (a -> e)
    a
  }

  def lookup(a: Address): Closure = cStore(a)

  def alpha: AStore = null
  def gamma: CStore = this

  override def toString = cStore.toString

  override def equals(a: Any) = a match {
    case s: CStore => cStore == s.cStore
    case _         => false
  }

  def copy(other: CStore) {
    cStore = other.cStore;
  }
}

object CStore {
  def apply() = new CStore()
  def copy(store: CStore): CStore = {
    var s = CStore();
    s.copy(store);
    return s;
  }
}


class AStore() extends Store {

  var aStore = Map[Address, Set[Closure]]();

  def aextendSingle(a: Address, e: Closure): Address = {
    if(aStore.contains(a)) {
      aStore += (a -> (aStore(a) + e));
    } else {
      aStore += (a -> Set(e));
    }
    return a;
  }

  def aextend(a: Address, e: Set[Closure]): Address = {
    if(aStore.contains(a)) {
      aStore += (a -> (aStore(a) ++ e));
    } else {
      aStore += (a -> e);
    }
    return a;
  }

  def alookup(e: Address): Set[Closure] = {
    return aStore(e);
  }

  def alpha: AStore = this
  def gamma: CStore = null

  override def toString = aStore.toString

  override def equals(a: Any) = a match {
    case s: AStore => aStore == s.aStore
    case _         => false
  }

  def copy(other: AStore) {
    aStore = other.aStore;
  }
}

object AStore {
  def apply() = new AStore()
  def copy(store: AStore): AStore = {
    var s = AStore();
    s.copy(store);
    return s;
  }
}

