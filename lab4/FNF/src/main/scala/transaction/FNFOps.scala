package transaction


object FNFOps {
    
    def isDependent(transactions: List[Transaction], element: Transaction): (String, List[Transaction]) = {
        (element.name, transactions.filter(t ⇒
            element.params.contains(t.result) ||
                t.params.contains(element.result)
        ))
    }
    
    def isNotDependent(transactions: List[Transaction], element: Transaction): (String, List[Transaction]) = {
        (element.name, transactions.filter(t ⇒
            !(element.params.contains(t.result) ||
                t.params.contains(element.result))
        ))
    }
    
    def dependencyRelation(transactions: List[Transaction]): Map[String, List[Transaction]] = {
        transactions.map(t ⇒ isDependent(transactions, t)).map {
            case (k: String, v: List[Transaction]) ⇒ k → v
        }.toMap
    }
    
    def independencyRelation(transactions: List[Transaction]): Map[String, List[Transaction]] = {
        transactions.map(t ⇒ isNotDependent(transactions, t)).map {
            case (k: String, v: List[Transaction]) ⇒ k → v
        }.toMap
    }
    def FNF(transactions: List[Transaction], possible: List[List[Transaction]]): List[List[Transaction]] = {
        transactions match {
            case Nil ⇒ possible
            case ::(head, tail) ⇒
                val possibleToVerify = independent(head.name).toSet
                val verified: List[Transaction] = verify(possibleToVerify, tail)
                FNF(verified.foldLeft(tail)((acc, e) ⇒ dropFirstMatch(acc, e)), List(List(head) ++ verified) ++ possible)
        }
    }
    
    def dropFirstMatch[A](ls: List[A], value: A): List[A] = {
        val index = ls.indexOf(value)
        if (index < 0) {
            ls
        } else if (index == 0) {
            ls.tail
        } else {
            val (a, b) = ls.splitAt(index)
            a ++ b.tail
        }
    }
    
    
    def verify(possibleToVerify: Set[Transaction], list: List[Transaction]): List[Transaction] = {
        list.foldLeft((possibleToVerify, List.empty[Transaction]))(
            (mergedAcc, head) ⇒ {
                if (possibleToVerify(head))
                    (mergedAcc._1 -- dependent(head.name), List(head) ++ mergedAcc._2)
                else
                    (mergedAcc._1 -- dependent(head.name), mergedAcc._2)
            })._2
    }
    
}
