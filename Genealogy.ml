(* Genealogy module body *)

(* 
Aluno 1: 58655 Paula Ines Lopes 
Aluno 2: 58403 Clara Sousa

Comment:

We have completed  13/13 functions and present each and every single on of them in this program.
For most methods in this project, we assumed that the repository can never be empty, 
having treated such case when meantioned in the project guide.
In certain auxiliary functions, however, unexpected cases (empty lists or reps) 
were also treated or presented in preconditions either for the sake of recursion, 
either to present a more logical reasoning.

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   100 columns
*)


(* AUXILIARY BASIC FUNCTIONS *)

let rec uniq l =
	match l with
	|  [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then uniq (y::xs)
		else x::uniq (y::xs)


let clean l = (* removes repetitions *)
	uniq (List.sort compare l)

let len =
	List.length

let map =
	List.map

let filter =
	List.filter

let mem =
	List.mem

let flatMap f l =
	List.flatten (map f l)

let partition =
	List.partition

let exists =
	List.exists

let for_all =
	List.for_all

let cFlatMap f l =
	clean (flatMap f l)

let union l1 l2 =
	clean (l1 @ l2)

let inter l1 l2 =
	filter (fun x -> mem x l2) l1

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1


(* ADDED AUXILIARY BASIC FUNCTIONS *)

let head =
	List.hd
;;
let tail =
	List.tl
;;

(* TYPES *)

type item = string * string list
type repository = item list

type aTree = ANil | ANode of string * aTree * aTree
type dTree = DNil | DNode of string * dTree list


(* EXAMPLES *)

(* Having developed over 10 examples, we have decided to present
only a fair few so as to keep the program short and simple 
*)

let example = [
  ("a", ["f";"g"]);
  ("b", ["f";"h"]);
  ("c", ["h";"i"]);
  ("f", ["g"; "j"]);
  ("g", ["j"]);
  ("h", []);
  ("i", []);
	("j", [])
]

let example1 = [ 
	("a", ["f";"g"]); 
	("b", ["f";"h"]); 
	("c", ["h";"i"]); 
	("f", ["g"; "j"]); 
	("g", ["j"]); 
	("h", ["x"]); 
	("i", []); 
	("j", ["y"]); 
	("x", []); 
	("y", []) ] 

let example2 = [ 
	("a", ["f";"j"]); 
	("f", ["j"; "g"]); 
	("j",[]); 
	("g",[]) 
]

let example3 = [ 
	("a", ["f";"g"]); 
	("b", ["f";"h"]); 
	("c", ["h";"i"]); 
	("f", ["g"; "j"]); 
	("g", ["j"; "w"]); 
	("h", []); 
	("i", []); 
	("j", ["w"; "s"]); 
	("w", ["s"]); 
	("s", []) 
] 

let example4 = [] 

let example5 = [
	("a", [])
]

let example6 = [ 
	("a", ["f";"g"]); 
	("b", ["f";"h"]); 
	("c", ["h";"i"]); 
	("f", ["g"; "j"]); 
	("g", ["j"]); 
	("h", ["x";"w"]); 
	("i", ["x"; "y"]); 
	("j", []); 
	("w", ["y";"z"]); 
	("x", ["w"]); 
	("y", ["z"]); 
	("z", []) 
] 

let example7 = [ 
	("f", ["s"]); 
	("s", [])
]

let example8 = [ 
	("a", ["x";"y"; "z"; "w"]); 
	("x", ["c1"]); 
	("y", ["c1"]); 
	("z", ["c2"]); 
	("w", ["c2"]); 
	("c1", []); 
	("c2", [])
]

let example9 = [
	("z", ["y"]);
	("y", ["x"]);
	("x", ["w"; "c"]);
	("w", ["a"; "b"]);
	("c", []);
	("a", []);
	("b", [])
	]

(* BASIC REPOSITORY FUNCTIONS *)

let size rep = (* number of individuals *)
	len rep

let all1 rep = (* all the individuals *)
	map fst rep	

let all2 rep = (* all the children (of anyone) *)
	cFlatMap snd rep
	
let roots rep = (* individuals without any parents *)
	diff (all1 rep) (all2 rep)

let inners rep = (* individuals with children *)
	let xs = filter (fun (p,cs) -> cs <> []) rep in
		all1 xs

let leaves rep = (* individuals without any children *)
	let xs = filter (fun (p,cs) -> cs = []) rep in
		all1 xs

let cut1 rep l = (* partition based on first component of the repository *)
	partition (fun (p,cs) -> mem p l) rep

let cut2 rep l = (* partition based on second component of the repository *)
	partition (fun (p,cs) -> inter cs l <> []) rep

let cut rep = (* partition -> (root pairs, rest pairs) *)
	cut1 rep (roots rep)

let children rep l = (* get all the children of the list l *)
	let (a,b) = cut1 rep l in
		all2 a

let rec parents rep l = (* get all the parents of the list l *)
	let (a,b) = cut2 rep l in
		all1 a

(* FUNCTION height *)

(* pre: validStructural rep && validSemantic rep *)
(* post: none *)
let rec height rep =
	if rep = [] then 0
	else 
			let (x,xs) = cut rep in
					1 + height xs
;;


(* FUNCTION merge *)

(* This function was moved to the top of the program
because it is used for some of the functions to be presented next 
*)

(* FUNCTION merge - Auxiliary Functions *)

(* getItem:
	Given a string (name of individual) that exists in two different repositories, 
	this function returns an ordered pair such as: 
	(name of indiviual, list of all its children in both repositories)
*)

(* pre: mem s rep1 &&  mem s rep2 *)
let getItem s rep1 rep2 =	
	(s, union(children rep1 [s])(children rep2 [s]))		
;;

(* oldItem:
	Given a string, returns an ordered pair such as:
	(name of individual, list of its children in the repository rep)
*)

(* pre: mem s rep *)
let oldItem s rep = 
	(s, children rep [s])
;;


(* FUNCTION merge - Main Function *)

(* pre: validStructural rep1 && validSemantic rep1 && validStructural rep2 && validSemantic rep2 *)
(* post: validStructural result *)
let rec merge rep1 rep2 = 
	(* Adding rep1 to rep2 *)
	match rep1 with
	| [] -> rep2
	| (x,y)::r1-> if ( mem x (all1 rep2)) 
									then (getItem x rep1 rep2)::(merge r1 (diff(rep2)([oldItem x rep2])))
								else (x,y)::merge r1 rep2
									
;;						


(* FUNCTION makeATree *)

(* pre: validStructural rep && validSemantic rep *)
(* post: none *)
let rec makeATree rep a =
	
if not (mem a (all1 rep)) then ANode(a, ANil,ANil)
	else 
		let p = parents rep [a] in 
			if p = [] then ANode(a, ANil, ANil)
			else 
				let parent1 = head (p) in 
				let parent2 = head(tail(p)) in 
 				ANode(a, makeATree rep parent1, makeATree rep parent2) 
;;

(* FUNCTION repOfATree *)

(* FUNCTION repOfATree - Auxiliary Function *)

(* getRootA:	
	Given an Atree, returns the string of one of its ANodes 
*)
let getRootA t = 
	match t with
	| ANil -> failwith "getRootA: empty tree"
	| ANode(x, _ ,_ ) -> x
;;

(* FUNCTION repOfATree - Main Function *)

(* pre: saneATree t *)
(* post: validStructural result *)
let rec repOfATree t = 
	match t with
	| ANil -> []
	| ANode(x, ANil,ANil) -> [(x,[])]
	| ANode(x, ANil, r) ->	merge ((getRootA r, [x]):: [(x, [])]) (repOfATree r)
	| ANode(x, l , ANil) -> merge ((getRootA l, [x]):: [(x, [])]) (repOfATree l)
	| ANode(x, l, r) -> 
		merge (merge ((getRootA l, [x]):: (getRootA r, [x])::[(x, [])]) (repOfATree l)) (repOfATree r)
;;

(* FUNCTION makeDTree *)

(* FUNCTION makeDTree - Auxiliary Methods *)  

(* ncons:
	Adding an NTree to the head of a list of NTrees in case such tree is not empty
*)
let ncons t l =
	if t = DNil then l 
	else t::l
;;
 
(* FUNCTION makeDTree - Main Method *)  

(* pre: validStructural rep && validSemantic rep *)
(* post: none *)
let rec makeDTree rep a = 
	
	if not (mem a (all1 rep)) then DNode (a, [])
		else DNode(a, dlist rep ( children rep [a]))
				
		and dlist rep lchildren = 
		match lchildren with 
		| [] -> []
		| c::cs -> ncons ( makeDTree rep c) (dlist rep cs)
;;


(* FUNCTION repOfDTree *)

(* FUNCTION repOfDTree - Auxiliary Functions *)

(* getRootD:	
	Given a Dtree, returns the string of one of its DNodes 
*)
let rec getRootD t = 
	match t with
	| DNil -> failwith "getRootD: empty tree"
	| DNode(x, _) -> x
;;

(* FUNCTION repOfDTree - Main Function *)

(* pre: saneDTree t *)
(* post: validStructural result *)
let rec repOfDTree t =
	match t with
	| DNil -> []
	| DNode(x , []) -> [(x, [])]
	| DNode(x, xs) ->  (x, map getRootD xs)::getNodeList xs 

	and getNodeList lst = 
	match lst with
	| [] -> []
	| n::nl  -> merge(repOfDTree n) (getNodeList nl)
;;


(* FUNCTION descendantsN *)

(* pre: validStructural rep && validSemantic rep && n >= 0 && noDuplicates lst *)
(* post: noDuplicates result *)
let rec descendantsN rep n lst =
	if(n = 0) then lst
	else descendantsN rep (n-1) (children rep lst)
;;


(* FUNCTION siblings *)

(* pre: validStructural rep && validSemantic rep && noDuplicates lst *)
(* post: noDuplicates result *)
let siblings rep lst =
  if (parents rep lst = []) then lst
	else clean(children rep (parents rep lst)) 
;;

(* FUNCTION siblingsInbreeding - Auxiliary Function *)

(* getParents:
	Given a list of children, returns each child's parents
	in a ordered pair such as: (parent, otherParent) 
*)
let rec getParents rep l = 
	match l with
	| [] -> []
  | x::xs ->  let otherParent = head(diff(parents rep [x])([head(parents rep [x])])) in
								let parent = head(diff(parents rep [x]) ([head(tail(parents rep [x]))])) in 
									(parent, otherParent)::getParents rep xs             
;;

(* sibsInBreeding:
	Given a list of individuals, returns all the pairs of parents 
	who share inbreeding children, in the format of list of ordered pairs 
*)
let rec sibsInbreeding rep l =
		match l with
		| [] -> []
		| x::xs -> let sibs = diff(siblings rep [x])([x]) in 
							 	if(sibs = []) then sibsInbreeding rep xs
								else 
									let  inbreed = inter(children rep [x])(children rep (sibs)) in 
			  					 (getParents rep inbreed) @ (sibsInbreeding rep xs)
;;

(* FUNCTION siblingsInbreeding - Main Function *)

(* In this function we used clean since it is possible for two 
	parents to have more than 1 child inbreeding, and have assumed 
	if several children have the same parents (from inbreeding), 
	it only makes	sense to present the pair or parents once
*)

(* pre: validStructural rep && validSemantic rep *)
(* post: noDuplicates result *)
let siblingsInbreeding rep =
	clean (sibsInbreeding rep ( diff(inners rep)(roots rep)) ) 
;;

(* FUNCTION waveN *)

(* pre: validStructural rep && validSemantic rep && n >= 0 && noDuplicates lst *)
(* post: noDuplicates result *)
let rec waveN rep n lst =
	if(n=0) then lst
	else 
		let wave = union (parents rep (waveN rep (n-1) lst)) (children rep (waveN rep (n-1) lst)) in 
			
			if( n = 1 ) then diff(wave)(lst)
			else
				let previousWaves = union(waveN rep (n-2) lst)(waveN rep (n-1) lst) in
				diff(wave)(previousWaves)
;;

(* FUNCTION supremum *)

(* Given the complexity of this function, we shall first attempt
	to present the reasoning used to solve it.
	
	Firstly, we calculate all the common ancestors of a list of individuals.
	If such list has only 1 element, that element 
	is to be returned, regardless of distance.
	
	Then, we start looking for the closest ancestor, starting from the roots that 
	are also common ancestors.

	From then on, our goal was to find a way to recursively search for the next
	ancestor(s) that is/are children of these "new level" ancestors.

	However, we could not successfully search through all levels 
	(sepremum does not work, for instance, for example9).

	As a result, we have decided to present the code that we believe works for some 
	cases/levels, and still present our attempt to solve the function at its best, 
	in commented code, at the end of "spremum".

	Last note: our code works any case where the closest ancestor 
	is directly below the roots that are also common ancestors.
*)


(* FUNCTION supremum - Auxiliary Functions *)

(* getAncestors:
	Returns all the ancestors of a given list of individuals
*)
let rec getAncestors rep lst = 
	let ancestors = parents rep lst in 			
	let newAncestors = union(parents rep ancestors)(ancestors) in
	if diff(newAncestors)(ancestors) = [] then ancestors 
	else 
		union(newAncestors)(getAncestors rep newAncestors)
;;

(* getCommonAncestors:
	Returns all the ancestors the individuals 
	in a given list have in common 
*)
let rec getCommomAncestors rep lst =
	match lst with
	| []-> [] 
	| x::xs -> 
		let firstAncestors = inter(getAncestors rep [x]) (getAncestors rep [head(xs)]) in
		if diff(xs)([head(xs)]) = [] then firstAncestors
		else inter(firstAncestors)(getCommomAncestors rep xs)				
;;

(* rootsChildrenList:
	Given the list, lst, of common ancestors that are only roots, 
	returns all the children of such elements.
*)
let rec rootsChildrenList rep lst =
	match lst with
	| []-> []
	| x::xs -> if( xs = []) then children rep [x]
	else 
		let firstDescendents = union(children rep [x]) (children rep [head(xs)]) in
			if diff(xs)([head(xs)]) = [] then firstDescendents
			else union(firstDescendents)(rootsChildrenList rep xs)
;;	
	
(* closerAncestors:
	Divides the given list of common ancestors into two groups:
	those that are roots (r) and those that are not (i), 
	and returns 
*)
let rec closerAncestors rep lst = 
	let r = inter(roots rep)(lst) in
	let i = diff(lst)(r) in
		inter(rootsChildrenList rep r)(i) 						
;;

(* FUNCTION supremum - Main Function *)

let supremum rep s =
 	let commomAncestors = getCommomAncestors rep s in
 	if( len(commomAncestors) = 1) then commomAncestors
 	else if( closerAncestors rep commomAncestors = []) then commomAncestors 
	else closerAncestors rep commomAncestors 
;;

(* HERE WE PRESENT THE COMMENTED CODE THAT AIMS TO WORK FOR MORE CASES/LEVELS *)

(*

(* FUNCTION supremum - Other Auxiliary Function *)

	let rec lastLevel rep comAnc currentLevel =
let firstLevelComAnc = closerAncestors rep comAnc in
	if inter (children rep firstLevelComAnc) (comAnc) = [] 
		then firstLevelComAnc
else lastLevel rep comAnc firstLevelComAnc
;;

(* FUNCTION supremum - Main Function *)

let supremum rep s =
 	let commomAncestors = getCommomAncestors rep s in
 	if( len(commomAncestors) = 1) then commomAncestors
 	else if( closerAncestors rep commomAncestors = []) then commomAncestors 
	(*firstLevelCommonAncestors = closerAncestors rep commomAncestors *)
	else 
		let firstLevelCommonAncestors = inter(roots rep)(commomAncestors)  in
		
		lastLevel rep commomAncestors firstLevelCommonAncestors
;;

*)
		
(* FUNCTION validStructural *)

(* FUNCTION validStructural - Auxiliary Functions *)

(* noDuplicate:
	Returns true if there are no duplicate individuals in the given list
*)
let rec noDuplicate lst = 
	match lst with
	| []-> true
	| x::xs -> if mem x xs then false
	else noDuplicate xs
;;

(* noFirstComponent: 
	Returns true if all the individuals in a given list exist 
	on the left component of a pair in the rep
*)
let rec noFirstComponent rep lst =
	match lst with
	| [] -> true
	| x::xs -> if not(mem x (all1 rep)) then false
	else noFirstComponent rep xs
;;

(* FUNCTION validStructural - Main Function *)

(* pre: none *)
(* post: none *)
let validStructural rep =
	noDuplicate (all1 rep) && noFirstComponent rep (all2 rep)
;;


(* FUNCTION validSemantic *)

(* FUNCTION validSemantic - Auxiliary Functions *)

(* checkParents:
	Returns true if all individuals in a list have 2 parents at most
*)
let rec checkParents rep lst = 
	match lst with
	| [] -> true
	| x::xs -> if len(parents rep [x]) > 2 then false
	else checkParents rep xs			
;;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        

(* checkAncestors:
	Returns true if each individual in a list never 
	belongs to its own group of ancestors
*)
let rec checkAncestors rep lst =
	match lst with
	| []-> true
	| x::xs -> if (mem x (getAncestors rep [x])) then false
	else checkAncestors rep xs
;; 	

(* FUNCTION validSemantic - Main Function *)

(* pre: validStructural rep *)
(* post: none *)
let validSemantic rep =
	let individuals = diff(all1 rep )(roots rep) in 
	checkParents rep individuals  &&  checkAncestors rep individuals
;;

(* End of a project made with happiness *)