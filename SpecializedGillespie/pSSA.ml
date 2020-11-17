let first(a,_) = a
let second(_,b) = b

let rec remove_dup list = match list with [] -> [] |
        h::tail -> if List.mem h tail
                   then remove_dup tail
                   else h::(remove_dup tail)


let rec remove a lst aux = match lst with 
                   [] -> failwith "This should not happen A" |
              h::tail -> if h = a
                         then aux@tail
                         else remove a tail (h::aux)

let rec removeB a lst aux = match lst with 
                         [] -> failwith "This should not happen B" |
                    h::tail -> if h = a
                               then aux@tail
                               else removeB a tail (h::aux)                         

let rec instance_remove (l1,r1)  state_reactions_domain_for_reaction  aux =
                 match state_reactions_domain_for_reaction with 
                                   [] -> aux |
                          (a,b)::tail -> if (a,b) = (l1,r1)
                                         then instance_remove (l1,r1) tail  aux
                                         else instance_remove (l1,r1) tail  ((a,b)::aux)          

(* ############################################################# *)


let qr p = print_string ((string_of_int p )^ " ") 

let pr (a,b,c) = List.iter qr a;
        print_string "-> ";
        List.iter qr b;
        print_string ((string_of_float c)^"\n")
              
(* 1 ==================================================== *)

let print_ir_spc (a,b) = print_string ((string_of_int a) ^ "," ^ (string_of_int b) ^ " ")        

let print_ir (a,b,c) = 
  List.iter print_ir_spc a;
  print_string " -> ";
  List.iter print_ir_spc b;
  print_string (": " ^ (string_of_float c) ^ "\n") 


let print_ir_comp (k,a,b) =
    print_string ((string_of_int k) ^ " - ");  
    List.iter print_ir_spc a;
    print_string " -> ";
    List.iter print_ir_spc b;
    print_string ("  ~  ") 
      
(* 2 ==================================================== *)

let print_rs_spc a = print_string ((string_of_int a) ^ " ")        

let print_rs (a,b,c) = 
  List.iter print_rs_spc a;
  print_string " -> ";
  List.iter print_rs_spc b;
  print_string (": "^(string_of_float c) ^ "\n") 

(* 3 ==================================================== *)

let rec print_ri_top lst n = match lst with [] -> () |
            h::tail -> print_string ("Reaction " ^ (string_of_int n) ^ 
                                     ": " ^ (string_of_int (List.length h)) ^ 
                                     " instantiations.\n");
                       print_ri_top tail (n+1)

(* 4 ==================================================== *)

let pr_list a = print_string ((string_of_int a) ^ " ") 

let rec get_first n lst = match lst with [] -> [] |
            h::tail -> if n = 0 
                       then []
                       else h::get_first (n-1) tail 

let spr a = for i=0 to (Array.length a)-1
              do  
              print_string ((string_of_int i) ^ " ~ ");
              (* print_string (string_of_int (List.length a.(i))); *)
              let this_state_list = a.(i) in
              let first20 = get_first 20 this_state_list in  
              List.iter pr_list first20;
              if List.length this_state_list > 20
              then print_string "...\n"    
              else print_string "\n"  
              done  

(* 5 ==================================================== *)

let print_activated_ri  state_reactions_domain = 
    for i = 1 to (Array.length state_reactions_domain)-1
    do
      print_string ("Reaction " ^ (string_of_int i) ^ ": ");
      print_string (string_of_int (List.length state_reactions_domain.(i)));
      print_string " instances.\n"
    done    

(* 6 ====================================================== *)

let print_state_counts  count_state  reac_species_array = 
       for i= 0 to (Array.length count_state)-1
       do
       print_string (reac_species_array.(i));
       print_string (": ");      
       print_string (string_of_int count_state.(i));
       if i = (Array.length count_state)-1 
       then print_string (".")
       else print_string (" ~ ")
       done

(* 7 ====================================================== *)

let print_a_j a_j = 
  for i= 0 to (Array.length a_j)-1
  do
    print_string ("a_" ^ (string_of_int i ) ^ ": ");  
    print_string (string_of_float a_j.(i));
    print_string ("     ")      
  done

(* ====================================================== *)  
(*                   Dependencies                         *)

let print_rs_spc0 a = print_string ((string_of_int a) ^ " ")        

let print_rs0 (a,b,c) = 
  print_string ("\n ==> "^(string_of_int a) ^ "\n");
  List.iter print_rs_spc0 b;
  print_string " -> ";
  List.iter print_rs_spc0 c
  
let print_dependencies dependencies = 
    for  i= 1 to (Array.length dependencies)-1
    do
      print_string ( "\nReaction:  "^ ( string_of_int i ) ^ " ~ ");
      List.iter print_rs0 (first dependencies.(i));
      print_string "\n" 
    done  

(* ====================================================== *)  
(*                Reaction domain                         *)

let  print_reaction_domain (a,b) = List.iter print_ir_spc a; 
                                   print_string " -> ";
                                   List.iter print_ir_spc b;
                                   print_string "\n" 

(* ====================================================== *)  
(*                Instance to add                         *)

let print_added_instances(k,l,r) = print_string (string_of_int k);
                                   print_string " ";
                                   List.iter  print_ir_spc l;
                                   print_string " -> ";
                                   List.iter  print_ir_spc r;
                                   print_string "\n"                                    
                     
(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)

open Bigarray
open Types

let current_time = ref 0.0
let simulation = ref []
let trace = ref [(0.,[],[])]
let tau = ref 0.
let mu = ref 0

(* ############################################################# *)

let rec all_available_in_current_state  left_lst  current_state = match left_lst with [] -> true |
         (a,b)::tail ->   if List.mem b current_state.(a) 
                          then all_available_in_current_state  tail  current_state
                          else false

(* ############################################################# *) 

let rec all_mem  sub_list super_list = match sub_list with [] -> true |
                h::tail -> if List.mem h super_list 
                           then all_mem tail super_list
                           else false 

(* ############################################################# *)  

let get_nodes reactions_lst = 
         let rec g_nodes_reactants lst = match lst with [] -> [] |
              (_,h)::tail -> h::g_nodes_reactants tail  
      in let rec g_nodes lst = match lst with [] -> [] |
   (a,b,c)::tail ->  (g_nodes_reactants a) @ g_nodes tail
      in List.sort compare (remove_dup (g_nodes reactions_lst)) 

(* ############################################################# *)      

let rec  get_net_consumption instance_left_lst  instance_right = 
             match instance_left_lst with [] -> [] |
                  h::tail ->  if List.mem  h  instance_right
                              then    get_net_consumption  tail  instance_right
                              else h::get_net_consumption  tail  instance_right 

(* ############################################################# *)      

let rec  get_net_production instance_left  instance_right_lst = 
  match instance_right_lst with [] -> [] |
       h::tail ->  if List.mem  h  instance_left
                   then    get_net_production  instance_left  tail
                   else h::get_net_production  instance_left  tail 

(* ############################################################# *)
(* ############################################################# *)

let initialize_nodes  nodes_array  indexed_reactions  reaction_schemes =
       let rec scheme_of lst = match lst with [] -> [] |
                                     (a,b)::tail -> a::scheme_of tail    
    in let rec get_reac_id lft rght schms_lst k = match  schms_lst with [] -> failwith "This should not happen C" |
                     (a,b,c)::tail ->   if (scheme_of lft,scheme_of rght) = (a,b)
                                        then (k,lft,rght)
                                        else get_reac_id lft rght tail (k+1)    
    in let rec add l_lst l r = match  l_lst with [] -> () |
                             (sp_id,node_id)::tail -> 
        nodes_array.(node_id) <-   (get_reac_id l r reaction_schemes 1)::(nodes_array.(node_id));
                                                  add tail l r     
    in let rec inn lst = match lst with [] -> () |
                      (left,right,r)::tail -> add left left right; 
                                       inn tail
    in inn  indexed_reactions                                    

(* ############################################################# *)
(* ############################################################# *)

let trace_to_file_txt  txt_output  sim species_array  =
        let channel1 = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 txt_output
  in let rec string_of lst = match lst with [] -> "" |
          (a,b)::tail ->  (species_array.(a) ^ "(" ^ (string_of_int b) ^ ")") ^ 
                          (if tail = [] then "" else " + ") ^
                          string_of tail    
  in let rec ptf_txt lst = match lst with
                         [] -> ()
  | (time,left,right)::tail ->
       output_string channel1 ( ( string_of_float time ) ^ " " ^
                                ( string_of left )   ^ " -> " ^
                                ( string_of right )  ^ "\n");
                                ptf_txt tail
  in let _ =  ptf_txt sim in let _ =  close_out channel1 in ()

(* ############################################################# *)

let print_to_file_csv  csv_output  sim  =
          let channel1 = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 csv_output
       in let rec string_of lst = match lst with [] -> "" |
                             h::tail ->  "," ^ ( string_of_int h ) ^ string_of tail
	     in let rec ptf_csv lst = match lst with
	                        [] -> ()
					  |  (time,this_state)::tail ->
					      output_string channel1 ( ( string_of_float time ) ^ ( string_of this_state )  ^ "\n");
							  ptf_csv tail
       in let _ =  ptf_csv sim in let _ =  close_out channel1 in ()

(* ############################################################# *)

let pd (x,y) = print_string ("" ^ (string_of_int x) ^"," ^ (string_of_int y) ^ " ~ ")
  
let print_domain (left,_right) = List.iter pd left; print_string "\n"

(* ############################################################# *)
             
let get_state_reactions_domain  state  reacs_instantiations = 
        let rec is_activated lst = match lst with [] -> true |
                            (spId,nodeId)::tail -> if List.mem  nodeId state.(spId)
                                                    then is_activated tail
                                                    else false  
      in let rec get_activated_cases  this_reac_insts = match this_reac_insts with [] -> [] |
          reac_instance::tail -> if is_activated (first reac_instance) 
                                then reac_instance::get_activated_cases tail
                                else get_activated_cases tail
      in List.map  get_activated_cases  reacs_instantiations
 
(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)

let apply reac_id  time  reaction_schemes_array  state_reactions_domain  state  count_state reac_species_array  nodes_array =
     let (left,right,_) = reaction_schemes_array.(reac_id) 
  in let rec apply_left lst = match lst with [] -> () |
         h::tail -> count_state.(h) <- count_state.(h) - 1; apply_left tail
  in let rec apply_right lst = match lst with [] -> () |
         h::tail -> count_state.(h) <- count_state.(h) + 1; apply_right tail
  (* ------------------------------------ *)
  in let rec apply_instance_left l_instance  = match l_instance with [] -> () |
                   (a,b)::tail -> state.(a) <- remove b state.(a) [];
                                               apply_instance_left  tail
  in let rec apply_instance_right r_instance  = match r_instance with [] -> () |
                   (a,b)::tail -> state.(a) <- b::(state.(a));
                                               apply_instance_right  tail                                  
  (* ------------------------------------ *)
  in let single_reac_domain = state_reactions_domain.(reac_id)
  in let reaction_domain = Array.of_list  single_reac_domain 
  in let domain_length = Array.length reaction_domain
  in let instance_id = Random.int domain_length
  in let (instance_left,instance_right) = reaction_domain.(instance_id)
  (* ------------------------------------ *) 
  in let _ = apply_instance_left  instance_left 
  in let _ = apply_instance_right instance_right
  (* ------------------------------------ *)
  in let _ = apply_left left;
  in let _ = apply_right right; 
  (* ==================================== *)
  in let rec  remove_instances_of_node_from_state_domain  (s,id) node_reac_list  = match node_reac_list with [] -> () |
                                 (k,l1,r1)::tail -> if List.mem  (s,id) l1
                                                    then state_reactions_domain.(k) <- 
                                                            (instance_remove (l1,r1) state_reactions_domain.(k) []);
                                                    remove_instances_of_node_from_state_domain  (s,id) tail
  in let removal_of_single_consumed (s,id) =  remove_instances_of_node_from_state_domain  (s,id)  nodes_array.(id)
  in let net_consumption = get_net_consumption instance_left instance_right  
  in let _ = List.iter removal_of_single_consumed net_consumption
  (* ==================================== *)
  in let rec add_for_node_instances  net_production  node_instances = match node_instances with [] -> () |
                                  (k,l1,r1)::tail  -> if (all_mem  net_production   l1)  && (all_available_in_current_state  l1 state)
                                                      then state_reactions_domain.(k) <-  ((l1,r1)::state_reactions_domain.(k));
                                                      add_for_node_instances  net_production  tail
  in let net_production = get_net_production instance_left instance_right
  in let net_produced_nodes = List.map second net_production 
  in let add_for_node nd = add_for_node_instances  net_production  nodes_array.(nd)
  in let _ = List.iter  add_for_node  net_produced_nodes
  (* ==================================== *)
  in let _ = trace := !trace @ [(time,instance_left,instance_right)]
  in ()
    
(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)

let next_action aj =
  let u = (Random.float aj.(0)) in
  let rec next countR n = if countR >= u
                      then n
            else next (countR +. aj.(n+1)) (n+1) in
     next 0. 0

(* ############################################################# *)

let compute_tau a0 =  ( 1. /. a0 ) *. ( log ( 1.0 /. (Random.float 1.0 ) ) )
(* tau = ( 1 / a0 )  * log ( 1 / r1 ) *)

(* ############################################################# *)

let compute_a_j_all  a_j reaction_schemes_array  state_reactions_domain =
  let third (_,_,c) = c in
  for i = 1 to (Array.length reaction_schemes_array) - 1
  do
    a_j.(i) <- (third reaction_schemes_array.(i)) *. (float_of_int (List.length state_reactions_domain.(i)))
  done
  ;
  a_j.(0) <- 0.
  ;
  for i = 1 to (Array.length a_j) - 1
  do
    a_j.(0) <- a_j.(0) +. a_j.(i)
  done

(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         
(* ############################################################# *)         


let rec get_observables_string lst = match lst with [] -> "" |
    h::tail -> h ^ (if tail = [] then "" else ", ") ^ get_observables_string tail 

(* ############################################################# *)         
(* ############################################################# *)         
  
let initialize_state reac_species init_species = 
      let rec check_is lst = match lst with 
                        [] -> [] | (a,1)::tail -> a::check_is tail |
                         _ -> failwith "Multiple instances of the same node."
   in let init_nodes = check_is init_species
   in let rec get_state_this this lst = match lst with [] -> [] |
                         (a,b)::tail -> if a = this
                                        then b::get_state_this this tail
                                        else    get_state_this this tail                         
   in let get_state this_spec = List.sort compare (get_state_this  this_spec  init_nodes)                            
   in let state = List.map  get_state  reac_species    
   in state

(* ############################################################# *)
(* ############################################################# *)

let get_reaction_scheme (a,b,c)  =
  let grs (x,_) = x in ((List.map grs a),(List.map grs b),c) 

let get_reaction_schemes lst = List.map get_reaction_scheme lst     
 
(* ############################################################# *)

let get_reaction_instantiations schemes_lst indexed_reacs_lst = 
         let rec gri_reac  scheme  lst = match lst with [] -> [] |
         (a,b,c)::tail -> if scheme = (get_reaction_scheme (a,b,c))
                          then (a,b)::gri_reac scheme tail
                          else        gri_reac scheme tail
      in let gri reac = gri_reac reac  indexed_reacs_lst
      in List.map gri schemes_lst

(* ############################################################# *)

let rec id_number n sp  species_lst = match species_lst with 
                        [] -> failwith "Error 1" |
                   s::tail -> if s = sp 
                              then n 
                              else id_number (n+1) sp tail

let index_reactions species reactions =
   let rec ir0 lst = match lst with [] -> [] |
         (h,n)::tail -> (id_number 0 h species,n)::(ir0 tail)
in let rec ir lst = match lst with [] -> [] |
      (_,left,right,rate)::tail -> (ir0 (List.sort compare left),ir0 (List.sort compare right),rate)::(ir tail)
in ir reactions

(* ############################################################# *)

let reaction_species reactions =
   let rec first_of lst = match lst with [] -> [] |
   (h,_)::tail -> h::first_of tail 
in let rec r_species list = match list with [] -> [] |
      (_,left,right,_rate)::tail -> ((first_of left) @ (first_of right)) @ (r_species tail)
in List.sort compare ( remove_dup ( r_species reactions) )

(* ############################################################# *)
(* ############################################################# *)
(* ############################################################# *)            
(* ====================================================== *)
(* ====================================================== *)
(* ====================================================== *)

let initialize reactions init observables csv_output txt_output = 
  let _ = print_string "Initializing the simulation.\n" in
  (* Preprocessing for initializing the simulation. *)
  let reac_species_list = List.sort compare (reaction_species reactions) in
  (* ---------------------------------------------- *)
  let indexed_reactions = index_reactions reac_species_list reactions in
  (* ---------------------------------------------- *)
  let init_species = List.sort compare init  in
  let state =  Array.of_list (initialize_state reac_species_list init_species) in
  (* ---------------------------------------------- *)
  let count_state = Array.map List.length state in 
  (* ---------------------------------------------- *)
  let channel1 = open_out csv_output in 
  let _ = output_string channel1 ("Time," ^ (get_observables_string reac_species_list) ^ "\n" );
          flush channel1;
          close_out channel1 in
  (* ---------------------------------------------- *)
  let channel2 = open_out txt_output in 
  let _ = output_string channel2 ("");
          flush channel2;
          close_out channel2 in
  (* ---------------------------------------------- *)
  (reac_species_list,indexed_reactions,state,count_state)
  
(* ######################################################### *)  
(* ######################################################### *)
(* ######################################################### *)  
(* ######################################################### *)

let run_sim (reactions,init,until,every,observables,interval) file_name  =
  let _ = print_string ("Initializing.\nExtracting the reaction dependencies.\n"); flush stdout in     
  let init_begin_time = Sys.time () in       
  (* ---------------------------------------------- *)          
  let csv_output =  file_name ^ ".csv"  in
  let txt_output =  "trace_" ^ file_name ^ ".txt"  in
  let (reac_species_list,indexed_reactions,state,count_state) = 
            initialize reactions init observables csv_output txt_output in
  (* ---------------------------------------------- *)          
  let reaction_schemes = remove_dup (get_reaction_schemes indexed_reactions) in 
  let reaction_schemes_array = Array.of_list (([],[],0.)::reaction_schemes) in  
  let reaction_instantiations = get_reaction_instantiations reaction_schemes indexed_reactions in
  let state_reactions_domain = Array.of_list ([]::get_state_reactions_domain state  reaction_instantiations) in 
  let observables_string = get_observables_string reac_species_list in
  let reac_species_array = Array.of_list reac_species_list in
  let nodes = get_nodes indexed_reactions in
  let number_of_nodes = List.length nodes in
  (* ---------------------------------------------- *)
  let nodes_array = Array.make (number_of_nodes+1) [] in
  let _ = initialize_nodes  nodes_array  indexed_reactions  reaction_schemes  in 
  (* ---------------------------------------------- *)
  let _ = print_string ("Initialization with " ^ (string_of_int number_of_nodes) 
      ^ " nodes took " ^ (string_of_float ((Sys.time ()) -. init_begin_time)) ^ " seconds.\n\n") in
  (* ---------------------------------------------- *)
(***) (* let _ = print_string "1 Indexed reactions:\n"; List.iter print_ir indexed_reactions; print_string "\n" in *)
(***) let _ = print_string ("1 Indexed reactions: " ^ (string_of_int (List.length indexed_reactions)) ^ " reactions\n\n") in
(***) let _ = print_string "2 Reaction schemes:\n"; List.iter print_rs reaction_schemes; print_string "\n" in
(***) let _ = print_string "3\n"; print_ri_top reaction_instantiations 1 in 
(***) let _ = print_string "Each instaniation has the reactant\nnode states and product node states.\n\n" in
(***) let _ = print_string "4 State:\n"; spr state; print_string "\n" in
(***) let _ = print_string "5 Activated reaction instances:\n"; 
              print_activated_ri  state_reactions_domain; print_string "\n"  in
(***) let _ = print_string ("6 Observables and initial counts: " ^ (observables_string) ^ "\n");  
              print_state_counts count_state reac_species_array; print_string "\n"  in             
  (* ---------------------------------------------- *) 
  let number_of_reactions = Array.length state_reactions_domain in
  let a_j = Array.make number_of_reactions 0. in
  let _ = Random.self_init () in
  let _ = compute_a_j_all  a_j  reaction_schemes_array  state_reactions_domain in
  let _ = print_string "\n7. Initial a_i:\n" in 
  let _ = print_a_j a_j in
  let cnt = ref 1 in 
  (* ######################################## *)
  (* ######################################## *)
  (* ######################################## *)
  let begin_time = Sys.time () in
  let _ = print_string "\n\nRunning...\n" in
          current_time := 0.;
          simulation :=  [(0., Array.to_list count_state)];
          flush stdout;
          while (!current_time < until) && ( a_j.(0) > 0. )
          do
            mu := next_action a_j;
            tau := compute_tau a_j.(0);
            current_time := !current_time +. !tau;
            apply  !mu  !current_time reaction_schemes_array  state_reactions_domain  state count_state reac_species_array  nodes_array;
            (* -------- *)                  
            compute_a_j_all  a_j  reaction_schemes_array  state_reactions_domain;
            simulation :=  !simulation @ [(!current_time, Array.to_list count_state)];
            if !cnt mod 1000 = 0 
            then (print_string ((string_of_int !cnt) ^ " steps.\n");
                  flush stdout); 
            cnt := !cnt + 1 
          done;
          print_string "\nDone.\n";
          print_to_file_csv  csv_output  !simulation;
          trace_to_file_txt  txt_output  !trace  reac_species_array;
          print_string((string_of_float ((Sys.time ()) -. begin_time)) ^ "\n") 
      
(* ######################################################### *)
(* ===================================================== *)
(* ===================================================== *)

let run file_name  =
        let crn = if file_name = "-help"
                  then ([],[],0.,0,[],0.)
                  else Lexer.parse_file file_name
	   (* ########################################################### *)
     in let (reactions,init,until,every,observables,interval) = crn
     (* ########################################################### *)
     in if file_name = "-help"
        then print_string ""
        else run_sim (reactions,init,until,every,observables,interval) file_name
                 
(* ################################################# *)
(* ################################################# *)
(* ################################################# *)

let _ = run Sys.argv.(1) 

