package pbdirect

object IdentityMaps {
  def emptyJavaIdentityMap[K, V]: java.util.Map[K, V] = new java.util.IdentityHashMap[K, V]
}
