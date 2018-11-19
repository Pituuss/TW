package transaction

case class Transaction(name: String, result: String, params: List[String]) {
    override def toString: String = name
}
