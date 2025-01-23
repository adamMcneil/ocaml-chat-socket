open Unix
open Thread

let default_host = "127.0.0.1"
let port = 8080
let ack_message = "###ack###"
let max_message_size = 1024
let mutex = Mutex.create ()

let send_messages socket =
  let rec send_message_loop message =
    if message <> "quit" then (
      Mutex.lock mutex;
      let start_time = Unix.gettimeofday () in
      ignore (write socket (Bytes.of_string message) 0 (String.length message));
      let buffer = Bytes.create max_message_size in
      ignore (read socket buffer 0 max_message_size);
      let end_time = Unix.gettimeofday () in
      let elapsed_time = end_time -. start_time in
      Printf.printf "%.6f seconds roundtrip latency\n%!" elapsed_time;
      Mutex.unlock mutex;

      let next_message = read_line () in
      send_message_loop next_message)
  in
  let message = read_line () in
  send_message_loop message;
  close socket

let ready_to_read socket =
  let read_fds, _, _ = Unix.select [ socket ] [] [] 0.0 in
  read_fds <> []

let recv_messages socket =
  let buffer = Bytes.create max_message_size in
  let rec recv_message_loop () =
    Mutex.lock mutex;
    if ready_to_read socket then (
      let bytes_read = read socket buffer 0 max_message_size in
      Printf.printf "Length of message: %d\n%!" bytes_read;
      let message = Bytes.to_string buffer in
      Printf.printf "Received: \"%s\"\n%!" message;
      ignore
        (write socket
           (Bytes.of_string ack_message)
           0
           (String.length ack_message));
      Mutex.unlock mutex;
      recv_message_loop ())
    else Mutex.unlock mutex;
    recv_message_loop ()
  in
  try recv_message_loop () with
  | Unix.Unix_error (Unix.EBADF, "select", _) -> Mutex.unlock mutex
  | e -> raise e

let start_server port =
  let socket_address = ADDR_INET (inet_addr_any, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  bind socket socket_address;
  listen socket 1;
  let rec accept_clients_loop () =
    Printf.printf "Server listening on port %d\n%!" port;
    let connected_socket, client_addr = accept socket in
    let _ = create send_messages connected_socket in
    let client =
      match client_addr with
      | ADDR_INET (addr, _) -> string_of_inet_addr addr
      | _ -> "Unknown"
    in
    Printf.printf "Client connected: %s\n%!" client;
    let _ = recv_messages connected_socket in
    Printf.printf "Client disconnected: %s\n%!" client;
    accept_clients_loop ()
  in
  accept_clients_loop ()

let connect_to_server host port =
  let socket_address = ADDR_INET (inet_addr_of_string host, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  connect socket socket_address;
  ignore (create recv_messages socket);
  Printf.printf "Connected to server %s:%d\n%!" host port;
  send_messages socket

let () =
  print_string "Do you want to be a server or client? (s/c) ";
  let server_option = read_line () in
  match server_option with
  | "s" -> start_server port
  | "c" -> (
      Printf.printf "Enter host to connect to: ";
      let host = read_line () in
      match host with
      | "" -> connect_to_server default_host port
      | _ -> connect_to_server host port)
  | _ -> Printf.printf "not a valid option"
