#load "graphics.cma";; 
Random.full_init;;

let open_graph(x) = Graphics.open_graph x ;;

let close_graph() = Graphics.close_graph() ;;

let clear_graph() = Graphics.clear_graph() ;;

let moveto(x,y) = Graphics.moveto x y ;;

let lineto(x, y) = Graphics.lineto x y ;;

let plot(x, y) = Graphics.plot x y ;;

let draw_circle(x, y, r) = Graphics.draw_circle x y r ;;

let draw_rect(x,y,dx,dy) = Graphics.draw_rect x y dx dy ;;

let set_color(color) = Graphics.set_color color ;;

let black = Graphics.black ;;
let blue = Graphics.blue ;;
let red = Graphics.red ;;
let green = Graphics.green ;;
let white = Graphics.white ;;
let yellow = Graphics.yellow ;;
let cyan = Graphics.cyan ;;
let magenta = Graphics.magenta ;;



let draw_rep(x,y,dx,dy,sx0,sx1,sx,sy0,sy1,sy) =
moveto(x, y) ; lineto(x+dx+50, y) ;
moveto(x, y) ; lineto(x, y+dy+50) ;
moveto(x+10, y-20) ; Graphics.draw_string sx0;
moveto(x+dx-10, y-20) ; Graphics.draw_string sx1;
moveto(x + dx/2, y-20) ; Graphics.draw_string sx;
moveto(x-20, y+10) ; Graphics.draw_string sy0;
moveto(x-20, y+dy-10) ; Graphics.draw_string sy1;
moveto(x -20, y+dy/2) ; Graphics.draw_string sy;;

open_graph " 800x800";;

draw_circle(400,400,400);;



let random (n : int ) : float = 
  ((float_of_int((Random.int ((n*2)*100)))-.(float_of_int(n*100)))/.100.)  
  
;;


let generate_point_x(n:int):float=
  random(n)
;;

let generate_point_y(n:int):float=
  random(n)
;;

let is_inside_circle(x,y,n : float*float*float ) : bool = 
  if ((x*.x)+.(y*.y)) <= (n*.n)
  then true
  else false
;;



let compute_pi (a,n:int*int) : string =
  let compteur_true : int ref = ref 0
  and compteur_false : int ref = ref 0
  and counter : int ref = ref a
  and coord_x : float ref = ref 0.
  and coord_y : float ref = ref 0.
  and mem_x : float ref = ref 0.
  and mem_y :float ref = ref 0.
  in
  (
  while !counter <> 0
    do
    
    print_float(!coord_y); 
    print_string("   ");
    print_float(!coord_x);
    print_string ("\n");
    
    coord_x := generate_point_x(n);
    coord_y := generate_point_y(n);
    if is_inside_circle(!coord_x,!coord_y,float_of_int(n)) = true
    then compteur_true := !compteur_true + 1
    else compteur_false := !compteur_false + 1;
    counter := !counter -1;
    (*
    draw_rect(((int_of_float((!coord_x +. 4.)))*100),((int_of_float((!coord_y +. 4.)))*100),2,2);
    *)
    plot(
    (int_of_float((!coord_x +. 4.))*100),
    ((int_of_float((!coord_y +. 4.))*100)
    ));
    moveto((int_of_float((!mem_x +. 4.))*100),
    (int_of_float((!mem_y +. 4.))*100));

    lineto((int_of_float((!coord_x +. 4.))*100),
    (int_of_float((!mem_y +. 4.))*100));
    
    mem_x := !coord_x;
    mem_y := !coord_y;
    done;
  String.concat " " ["Pi ~="; (string_of_float(((float_of_int(!compteur_true))/.float_of_int(a))*.4.));"\n"]
  )

;;

let result = compute_pi(300,4) in print_string result
