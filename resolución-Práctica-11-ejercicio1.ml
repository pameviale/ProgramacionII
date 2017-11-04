
open List;

(* PRÁCTICA 11 *)

(*———————————————*)

(* EJERCICIO 1 *)

type estudiante = {Nombre:string, Notas: real list};

type estudiante_prom = {Nombre:string, Promedio: real};

(* A *)
(* val sumareal = fn : real * real -> real*)
fun sumareal(x:real,y)= x+y;

(*val promedio = fn : real list -> real*)
fun promedio(l: real list) = 
	let 
		val sumanotas =  foldr sumareal 0.0 l;
		val cantnotas =  length l;
	in 
		sumanotas/real(cantnotas)
	end;

(* val promedio_estudiante = fn : estudiante -> estudiante_prom *)
fun promedio_estudiante(est: estudiante) = {Nombre = #Nombre(est), Promedio=promedio(#Notas(est))}:estudiante_prom;

(* val promedios = fn : estudiante list -> estudiante_prom list*)
fun promedios(lista: estudiante list) = map promedio_estudiante lista;


(* B *)
(* val aprobado = fn : estudiante_prom -> bool *)
fun aprobado(est: estudiante_prom) = (#Promedio(est)>=6.0);

(* val aprobados = fn : estudiante list -> int *)
fun aprobados(lista: estudiante list) = length(filter aprobado (promedios lista));

val l= [{Nombre= "Paola", Notas=[1.0,7.0,4.5]}, {Nombre= "Alejo", Notas=[10.0,7.0,9.5]}]:estudiante list;

val prom=promedios(l);
val aprob=aprobados(l);
