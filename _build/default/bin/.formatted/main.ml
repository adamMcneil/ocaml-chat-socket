let port = 8080

let send_messages client_socket =
  let message = read_line () in
  let bytes = Bytes.of_string message in
  let _ = Unix.send client_socket bytes 0 (Bytes.length bytes) [] in
  ()

let recv_messages socket =
  let buffer = Bytes.create 1024 in
  let bytes_received = Unix.recv socket buffer 0 1024 [] in
  Printf.printf "Received: %s\n" (Bytes.sub_string buffer 0 bytes_received);
  ()

let start_server () =
  print_endline "starting server";

  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let address = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in
  let _ = Unix.bind socket address in
  let _ = Unix.listen socket 1 in
  let client_socket, _ = Unix.accept socket in
  recv_messages client_socket

let start_client () =
  print_string "What ip do you want to connect to? ";
  let server_ip = read_line () in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let address = Unix.ADDR_INET (Unix.inet_addr_of_string server_ip, port) in
  let _ = Unix.connect socket address in
  send_messages socket

let () =
  print_string "Do you want to be a server or client? (s/c)";
  let server_option = read_line () in
  match server_option with
  | "s" -> start_server ()
  | "c" -> start_client ()
  | _ -> Printf.printf "not a valid option"
