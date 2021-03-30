(* vec3d type, can represent a 3d vector or a color *)
type vec3d = {x: float; y: float; z: float}
let vec3d x y z: vec3d = {x = x; y = y; z = z};; 
(* vector addition *)
let ( +| ) v1 v2 = {x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z};;
(* vector subtraction *)
let ( -| ) v1 v2 = {x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z};;
(* scalar - vector subtraction *)
let ( -- ) m v1 = {x = m -. v1.x; y = m -. v1.y; z = m -. v1.z};;
(* element wise vector multiplication *)
let ( *| ) v1 v2 = {x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z};;
(* vector scalar multiplication *)
let ( ** ) v1 m = {x = v1.x *. m; y = v1.y *. m; z = v1.z *. m};;
(* vector scalar division *)
let ( // ) v1 m = v1 ** (1.0 /. m);;
(* dot product *)
let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z;;
(* length of the vector squared *)
let lengthsq v1 = v1.x *. v1.x +. v1.y *. v1.y +. v1.z *. v1.z;;
let length v1 = sqrt(lengthsq v1);;
let normalize v1 = v1 // (length v1);;
let print_v3 v1 = Printf.printf "%f %f %f\n" v1.x v1.y v1.z;;
let vec3d_zero = vec3d 0. 0. 0.;;
let is_zero v = v.x = 0. && v.y = 0. && v.z = 0.;;

let eye = vec3d_zero;;
let forward = vec3d 0. 0. (-1.);;
let right = vec3d 1. 0. 0.;;
let up = vec3d 0. 1. 0.;;

type ray = {origin: vec3d; direction: vec3d};;
let ray org dir = {origin = org; direction = dir};;

type light = {direction: vec3d; color: vec3d;};;
let light dir color = {direction = dir; color = color};;

type sphere = {
    radius: float;
    roughness: float;
    center: vec3d;
    color: vec3d;
    shininess: vec3d;
}

type plane = {
    a: float;
    b: float;
    c: float;
    d: float;
    roughness: float;
    color: vec3d;
    shininess: vec3d;
}

type global = {
    mutable spheres : sphere list;
    mutable planes : plane list;
    mutable sunlights: light list;
    mutable point_lights: light list;
    mutable bounces: int;
}

let globals = {
    spheres = [];
    planes = [];
    sunlights = [];
    point_lights = [];
    bounces = 4;
}

(* ray plane intersection. return tc such that intersection point = ry.origin +| ry.direction ** tc *)
let ray_plane_ins (ry: ray) pl =
    let normal = vec3d pl.a pl.b pl.c in
    let temp = dot ry.direction normal in
    let p = 
        if pl.a <> 0.0 then vec3d (-.pl.d /. pl.a) 0. 0.
        else if pl.b <> 0.0 then vec3d 0. (-.pl.d /. pl.b) 0.
        else vec3d 0. 0. (-.pl.d /. pl.c) in
    (dot (p -| ry.origin) normal) /. temp;;

(* ray sphere intersection. return tc such that intersection point = ry.origin +| ry.direction ** tc *)
let ray_sphere_ins ry sp = 
    let temp = sp.center -| ry.origin in
    let rsq = sp.radius *. sp.radius in
    let inside = (lengthsq temp) < rsq in
    let tc = dot temp ry.direction in
    
    let dsq = lengthsq (ry.direction ** tc -| temp) in
    let toffset = sqrt (rsq -. dsq) in

    match inside with
    | false -> (
        match ((tc < 0.) || (dsq > rsq)) with
        | true -> -1.0 (* no intersection *)
        | false -> tc -. toffset)
    | true -> tc +. toffset;;

type ins_info = {
    tc: float;
    roughness: float;
    ins_point: vec3d;
    color: vec3d;
    normal: vec3d;
    shininess: vec3d;
}

(* find the nearest intersection object and return its properties (e.g. color, shininess, etc.) *)
let nearest_obj ry = 
    let (msptc, msp) = List.fold_left (fun (mtc, msp) sp -> 
        let tc = ray_sphere_ins ry sp in
        match tc > 1e-8 && tc < mtc with
        | true -> (tc, sp)
        | false -> (mtc, msp)
        ) (1e8, {center = vec3d_zero; radius = 0.0; color = vec3d_zero; shininess = vec3d_zero; roughness = 0.0}) globals.spheres in
    
    let (mpltc, mpl) = List.fold_left (fun (mtc, mpl) pl -> 
        let tc = ray_plane_ins ry pl in
        match tc > 1e-8 && tc < mtc with
        | true -> (tc, pl)
        | false -> (mtc, mpl)
        ) (1e8, {a = 0.0; b = 0.0; c = 0.0; d = 0.0; color = vec3d_zero; shininess = vec3d_zero; roughness = 0.0}) globals.planes in
    
    let mtc = min msptc mpltc in
    let ins_point = ry.origin +| ry.direction ** mtc in
    (* depending on whether the nearest object is a plane or a sphere, calculate/retrieve the object properties *)
    match mtc with
    | _ when mtc = msptc -> {tc = mtc; ins_point = ins_point; color = msp.color; normal = ins_point -| msp.center; shininess = msp.shininess; roughness = msp.roughness}
    | _ -> {tc = mtc; ins_point = ins_point; color = mpl.color; normal = vec3d mpl.a mpl.b mpl.c; shininess = mpl.shininess; roughness = mpl.roughness}

(* sample from a normal distribution with mean=0 and std=sigma *)
(* this is the Box-Muller method. see https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform  *)
let norm_dist sigma =
    let u = Random.float 1. and v = Random.float 1. in
    sigma *. sqrt(-2. *. (log u)) *. cos(2. *. Float.pi *. v)

(* trace a ray. returns the return color of the ray *)
let rec trace_ray ry d =
    let overall_color = vec3d_zero in
    let result = nearest_obj(ry) in
    match result.tc with 
    | 1e8 -> (overall_color, false)
    | _ -> (
        let normal = normalize result.normal in
        let normal = (*normal should point towards the eye*)
            match (dot normal ry.direction) > 0. with
            | true -> normal ** -1.0
            | false -> normal in

        let normal = 
        match result.roughness with
        | 0.0 -> normal
        | _ -> (*randomly perturb normal to simulate roughness*)
            let per_vec = vec3d (norm_dist result.roughness) (norm_dist result.roughness) (norm_dist result.roughness) in 
            normalize (normal +| per_vec) in
        
        let sunlights_color = List.fold_left (
            fun sum_color light -> 
                let {tc;_} = nearest_obj(ray result.ins_point light.direction) in
                match tc with
                | 1e8 -> (*accumulate color if no intersection*)
                    sum_color +| result.color *| light.color ** (max 0.0 (dot light.direction normal)) 
                | _ -> sum_color (*some obj blocks the light -> shadow*)
            ) (vec3d_zero) globals.sunlights and

        pointlights_color = List.fold_left (
            fun sum_color light -> 
                let light_dir = light.direction -| result.ins_point in
                let lsq = lengthsq light_dir in
                let falloff = 1. /. lsq in
                let n_light_dir = normalize light_dir in
                let {tc;_} = nearest_obj(ray result.ins_point n_light_dir) in
                match tc with (*accumulate color if no intersection or intersection occurs after the point light*)
                | _ when (tc = 1e8) || (tc > (sqrt lsq)) -> 
                    sum_color +| result.color *| light.color ** (falloff *. (max 0.0 (dot n_light_dir normal))) 
                | _ -> sum_color (*some obj blocks the light -> shadow*)
            ) (vec3d_zero) globals.point_lights in

        let overall_color = overall_color +| sunlights_color +| pointlights_color in

        let overall_color = 
            match not (is_zero result.shininess) && d < globals.bounces with
            | true -> (*recursively trace reflected rays*)
                let rel_dir = ry.direction -| normal ** (2. *. (dot ry.direction normal)) in
                let (return_color, _) = trace_ray (ray result.ins_point rel_dir) (d + 1) in
                (* note: if no intersection, the return color is black *)
                overall_color *| (1. -- result.shininess) +| result.shininess *| return_color
            | false -> (*no reflection or reflection limit exceeded*)
                overall_color in 
        (overall_color, true)
    );;

(* from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
let read_file filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;
(*end from*)

open Bimage
(*the main procedure. note that main is not a function*)
let main = 
    let lines = read_file Sys.argv.(1) in
    let width = ref 0 and height = ref 0 in
    let filename = ref "" in
    Scanf.sscanf (List.hd lines) "%s %d %d %s" (
        fun _ w h fn -> 
            width := w; height := h; filename := fn);
    print_string "generating ";
    print_endline !filename;

    let color = ref (vec3d 1. 1. 1.) in
    let shininess = ref vec3d_zero in
    let roughness = ref 0.0 in

    List.iter (fun line -> 
        let cmd = ref "" in
        Scanf.sscanf line "%s " (fun s -> cmd := s);
        match !cmd with
        | "" -> () (*empty line, do nothing*)
        | "sphere" -> 
            Scanf.sscanf line "%s %f %f %f %f" (fun _ a b c d ->
                globals.spheres <- {center = {x = a; y = b; z = c;}; radius = d; color = !color; shininess = !shininess; roughness = !roughness} :: globals.spheres
            );
        | "plane" -> 
            Scanf.sscanf line "%s %f %f %f %f" (fun _ a b c d ->
                globals.planes <- {a = a; b = b; c = c; d = d; color = !color; shininess = !shininess; roughness = !roughness} :: globals.planes
            );
        | "sun" -> 
            Scanf.sscanf line "%s %f %f %f" (fun _ a b c -> 
                globals.sunlights <- (light (normalize({x = a; y = b; z = c;})) !color) :: globals.sunlights
            )
        | "color" ->
            Scanf.sscanf line "%s %f %f %f" (fun _ a b c -> color := {x = a; y = b; z = c;})
        | "bulb" ->
            Scanf.sscanf line "%s %f %f %f" (fun _ a b c -> 
                globals.point_lights <- (light {x = a; y = b; z = c;} !color) :: globals.point_lights
            )
        | "shininess" -> ( (*there are 2 variants of shininess*)
            try
                Scanf.sscanf line "%s %f %f %f" (fun _ a b c -> shininess := {x = a; y = b; z = c;});
            with End_of_file ->
                Scanf.sscanf line "%s %f" (fun _ a -> shininess := {x = a; y = a; z = a;})
            )
        | "roughness" ->
            Scanf.sscanf line "%s %f" (fun _ a -> roughness := a);
        | "bounces" ->
            Scanf.sscanf line "%s %d" (fun _ a -> globals.bounces <- a);
        | _ -> Printf.printf "Unknown command %s\n" !cmd
        ) (List.tl lines);
    
    (* create a rgba image *)
    let img = Image.create u8 rgba !width !height in
    let clamp a min_v max_v = min max_v (max a min_v) in
    let _ =
        let w = float_of_int img.width and h = float_of_int img.height in
        let maxWH = max w h in
        
        Image.for_each (fun x y _px ->
            (*shoot a ray for each pixel*)
            let _x = float_of_int x and _y = float_of_int y in
            let sx = (2. *. _x -. w) /. maxWH in
            let sy = (h -. 2. *. _y) /. maxWH in
            let dir = normalize (forward +| right ** sx +| up ** sy) in

            let (ry_color, has_ins) = trace_ray (ray eye dir) 0 in
            
            match has_ins with
            | true -> 
                Image.set img x y 0 (clamp (int_of_float (ry_color.x *. 255.)) 0 255);
                Image.set img x y 1 (clamp (int_of_float (ry_color.y *. 255.)) 0 255);
                Image.set img x y 2 (clamp (int_of_float (ry_color.z *. 255.)) 0 255);
                Image.set img x y 3 255
            | false ->
                Image.set img x y 0 0;
                Image.set img x y 1 0;
                Image.set img x y 2 0;
                Image.set img x y 3 0
        ) img
    in

    (* Save the image using ImageMagick *)
    Bimage_unix.Magick.write !filename img;;