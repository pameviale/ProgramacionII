open List;

(* ——————————————————————————————————*)

(* Ejercicio 1 *)

datatype 'b arbol2 = Nodo of 'b * 'b arbol2 list;
val t=Nodo("Jorge", [Nodo("Juan", [ Nodo("Esteban",nil)]), Nodo("Facundo",nil), Nodo("Renzo",nil)]);

(* ——————————————————————————————————*)

(* Ejercicio 2 *)

(* A *)
fun cantAparece(Nodo(n,nil),s:string) = if(n=s) then 1 else 0
| cantAparece(Nodo(n,x::xs),s) = cantAparece(x,s) + cantAparece(Nodo(n,xs),s);

(* B *)
fun firstLetter(Nodo(n,nil):(string arbol2)) = Nodo(hd(explode(n)),nil):(char arbol2)
| firstLetter(Nodo(n,x::xs)) = Nodo(hd(explode(n)),firstLetter(x)::firstLetterList(xs))
and
firstLetterList(nil) = nil
| firstLetterList(x::xs) = firstLetter(x)::firstLetterList(xs); 

(* ——————————————————————————————————*)

(* Ejercicio 3 *)
(* A *)
fun opAbs(x:int) = ~1*abs(x);

fun listNeg(l:int list) = map opAbs l;

listNeg([1,0,~7,4,~9]);

(* B *)

fun may5(x:int) = x>5;
fun prod(x,y) = x*y; 

fun prodMay5(l:int list) = (foldr prod 1 (filter may5 l));
prodMay5([~1,0,7,5,4,9]);

(* ——————————————————————————————————*)

(* Ejercicio 4 *)

datatype 'etiqueta arbolbin = Vacio | Nodo2 of  'etiqueta * 'etiqueta arbolbin * 'etiqueta arbolbin;

type paciente = {Nombre: string, Edad: int, Enfermedad: string, Medico: string};

(* A *)

fun crear_paciente(nom:string,ed:int, enf:string, med:string) = {Nombre=nom,Edad=ed, Enfermedad=enf,Medico=med}:paciente;


fun ingresar_paciente(Vacio: paciente arbolbin, nom:string,ed:int, enf:string, med:string)=
	Nodo2(crear_paciente(nom,ed,enf,med),Vacio,Vacio)
| ingresar_paciente(Nodo2(n, izq,der): paciente arbolbin, nom:string,ed:int, enf:string, med:string)= 
        if(#Nombre(n) = nom) then Nodo2(crear_paciente(nom,ed,enf,med),izq,der)
		            else (if (#Nombre(n)> nom) then Nodo2(n,ingresar_paciente(izq, nom,ed,enf,med),der) 
                                                      else Nodo2(n,izq,ingresar_paciente(der, nom,ed,enf,med)));

(* B *)

fun inOrden(Vacio) = nil
| inOrden(Nodo2(n, izq, der)) = inOrden(izq) @ [n] @ inOrden(der);

fun atiende_medico (med:string) (pac:paciente) = #Medico(pac) = med; 


fun buscar_pacientes_medico(ar:paciente arbolbin,med:string) = filter (atiende_medico med) (inOrden(ar));

(* C *)

fun actualizar_diag_enfermedad(Vacio: paciente arbolbin, nom:string, enf:string)= Vacio
| actualizar_diag_enfermedad(Nodo2(n, izq,der): paciente arbolbin, nom:string, enf: string)= 
        if(#Nombre(n) = nom) then Nodo2(crear_paciente(nom,#Edad(n),enf,#Medico(n)),izq,der)
		            else (if (#Nombre(n)> nom) then Nodo2(n,actualizar_diag_enfermedad(izq, nom,enf),der) 
                                                      else Nodo2(n,izq,actualizar_diag_enfermedad(der, nom,enf)));

(* D *)


fun nombre(pac:paciente) = #Nombre(pac);

fun edad_en_rango (min:int) (max:int) (pac:paciente) = #Edad(pac)>=min andalso #Edad(pac)<=max; 

fun buscar_pacientes_edad(ar: paciente arbolbin, min:int, max:int) = 
	let
		val lista = inOrden(ar)
	in 
		filter (edad_en_rango min max) lista
	end;


fun buscar_nombres_edad(ar: paciente arbolbin, min:int, max:int) = 
map nombre (buscar_pacientes_edad(ar,min,max));

(* E *)


fun enfermedad(pac:paciente) = #Enfermedad(pac);

fun tiene_enfermedad (enf: string) (pac: paciente) = #Enfermedad(pac) = enf;

fun buscar_pacientes_enfermedad(pacarbol: paciente arbolbin, enf:string) = 
	let
		val lista=inOrden(pacarbol);
	in
		filter (tiene_enfermedad enf) lista
	end;

