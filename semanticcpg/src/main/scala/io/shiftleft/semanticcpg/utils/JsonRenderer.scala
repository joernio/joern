package io.shiftleft.semanticcpg.utils

/** Renders arbitrary values to JSON, based on ujson.
  *
  *   - custom serializers are consulted first, in the order they are given
  *   - case classes (and other `Product`s) are rendered as objects with their product elements as fields
  *   - `None` values are omitted from objects and arrays, `Some` values are unwrapped
  *   - tuples with a `String` (or `Symbol`) first element are rendered as single-field objects
  *   - maps are rendered as objects with their keys converted via `toString`
  *   - all other values are rendered via `toString`
  *
  * @param customSerializers
  *   serializers for types that require custom handling, e.g. to only render a subset of their properties
  */
class JsonRenderer(customSerializers: List[JsonRenderer.Serializer] = Nil) {

  private lazy val serializers: List[PartialFunction[Any, ujson.Value]] =
    customSerializers.map(_.apply(this))

  def render(value: Any): ujson.Value = {
    serializers.view.map(_.lift).flatMap(_(value)).headOption.getOrElse(renderDefault(value))
  }

  private def renderDefault(value: Any): ujson.Value = value match {
    case null                          => ujson.Null
    case v: ujson.Value                => v
    case s: String                     => ujson.Str(s)
    case b: Boolean                    => ujson.Bool(b)
    case i: Int                        => ujson.Num(i)
    case l: Long                       => ujson.Num(l.toDouble)
    case d: Double                     => ujson.Num(d)
    case f: Float                      => ujson.Num(f.toDouble)
    case bi: BigInt                    => ujson.Num(bi.doubleValue)
    case bd: BigDecimal                => ujson.Num(bd.doubleValue)
    case o: Option[?]                  => o.map(render).getOrElse(ujson.Null)
    case m: scala.collection.Map[?, ?] => renderMap(m)
    case it: Iterable[?]               => renderIterable(it)
    case a: Array[?]                   => renderIterable(a.toList)
    case c: java.util.Collection[?]    => renderIterable(scala.jdk.CollectionConverters.CollectionHasAsScala(c).asScala)
    case (k: String, v)                => renderMap(Map(k -> v))
    case (k: Symbol, v)                => renderMap(Map(k.name -> v))
    case p: Product                    => renderProduct(p)
    case other                         => ujson.Str(other.toString)
  }

  private def renderMap(map: scala.collection.Map[?, ?]): ujson.Obj = {
    val obj = ujson.Obj()
    map.foreach { case (k, v) =>
      renderField(v).foreach(obj(k.toString) = _)
    }
    obj
  }

  private def renderProduct(product: Product): ujson.Obj = {
    val obj = ujson.Obj()
    product.productElementNames.zip(product.productIterator).foreach { case (name, value) =>
      renderField(value).foreach(obj(name) = _)
    }
    obj
  }

  private def renderIterable(it: Iterable[?]): ujson.Arr = {
    val arr = ujson.Arr()
    it.foreach(element => renderField(element).foreach(arr.value += _))
    arr
  }

  /** Renders an object field or array element: `None` is omitted and `Some` is unwrapped. */
  private def renderField(value: Any): Option[ujson.Value] = value match {
    case None    => None
    case Some(x) => Some(render(x))
    case other   => Some(render(other))
  }

}

object JsonRenderer {

  /** A custom serializer for use with [[JsonRenderer]]: given the renderer (for rendering nested values), returns a
    * partial function that renders the values it is defined for.
    */
  type Serializer = JsonRenderer => PartialFunction[Any, ujson.Value]
}
