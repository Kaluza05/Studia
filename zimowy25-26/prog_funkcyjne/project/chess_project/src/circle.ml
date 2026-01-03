open Graphics

let () =
  (* otwarcie okna 400×400 *)
  open_graph " 400x400";

  (* ustawiamy kolor *)
  set_color yellow;

  (* rysujemy wypełnione kółko:
     (x=200, y=200), promień = 80 *)
  fill_circle 200 200 80;

  (* czekamy na klawisz żeby okno nie zamknęło się od razu *)
  ignore (read_key ())