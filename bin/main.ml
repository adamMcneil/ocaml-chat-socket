open Unix
open Thread

let host = "127.0.0.1"
let port = 8080
let ack_message = "###ack###"
let max_message_size = 1024
(* let messages = [] *)
(* let messages_mutex = Mutex.create () *)

let send_messages socket =
  let rec loop message =
    if message <> "quit" then (
      let start_time = Unix.gettimeofday () in
      ignore
        (Unix.write socket (Bytes.of_string message) 0 (String.length message));
      let buffer = Bytes.create max_message_size in
      let _ = Unix.read socket buffer 0 max_message_size in
      let end_time = Unix.gettimeofday () in
      let elapsed_time = end_time -. start_time in
      Printf.printf "Server response: %.6f seconds\n%!" elapsed_time;
      let buffer_string = Bytes.to_string buffer in
      print_endline buffer_string;
      let next_message = read_line () in
      loop next_message)
  in
  let message = read_line () in
  loop message;
  Printf.printf "closing socket%!";
  close socket

let recv_messages socket =
  let buffer = Bytes.create max_message_size in
  let rec loop () =
    let bytes_read = read socket buffer 0 max_message_size in
    let message = Bytes.sub_string buffer 0 bytes_read in
    match message with
    (* | "###ack###" -> *)
    (*     Printf.printf "recieved ack at the wrong time\n%!"; *)
    (*     loop () *)
    | "" ->
        Printf.printf "closing recv ending%!";
        close socket
    | _ ->
        Printf.printf "Received: \"%s\"\n%!" message;
        let _ =
          write socket
            (Bytes.of_string ack_message)
            0
            (String.length ack_message)
        in
        loop ()
  in
  loop ()

let start_server port =
  let socket_address = ADDR_INET (inet_addr_any, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  bind socket socket_address;
  listen socket 1;
  Printf.printf "Server listening on port %d\n%!" port;
  let rec loop () =
    let connected_socket, client_addr = accept socket in
    let _ = create send_messages connected_socket in
    Printf.printf "Client connected: %s\n%!"
      (match client_addr with
      | ADDR_INET (addr, _) -> string_of_inet_addr addr
      | _ -> "Unknown");
    let _ = recv_messages connected_socket in
    Printf.printf "Disconnected\n%!";
    loop ()
  in
  loop ()

let connect_to_server host port =
  let socket_address = ADDR_INET (inet_addr_of_string host, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  connect socket socket_address;
  ignore (create recv_messages socket);
  Printf.printf "Connected to server %s:%d\n%!" host port;
  send_messages socket

let () =
  print_string "Do you want to be a server or client? (s/c)";
  let server_option = read_line () in
  match server_option with
  | "s" -> start_server port
  | "c" -> connect_to_server host port
  | _ -> Printf.printf "not a valid option"
