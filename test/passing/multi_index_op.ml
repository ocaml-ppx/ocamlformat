let ( .%{;..} ) = Genarray.get

let ( .%{;..}<- ) = Genarray.set

let () =
  let x = Genarray.create Float64 c_layout [|3; 4; 5|] in
  x.%{0; 0; 0} <- 3. ;
  Printf.printf "%f\n" x.%{0; 0; 0}
