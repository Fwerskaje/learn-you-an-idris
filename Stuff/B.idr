module B

andb_commutative : (n : Bool) -> (c : Bool) -> b && c = c && b
andb_commutative False False = ?andb_commutative_rhs_5
andb_commutative False True = ?andb_commutative_rhs_4
andb_commutative True False = ?andb_commutative_rhs_1
andb_commutative True True = ?andb_commutative_rhs_3

