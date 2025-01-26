open Unix
open Thread

let default_host = "127.0.0.1"
let port = 8080
let ack_message = "###ack###"
let max_message_size = 1024
let mutex = Mutex.create ()
let socket_is_closed = ref false
let quit = ref false

let send_file file_name socket =
  let file = open_in_bin file_name in
  let buffer_size = max_message_size in
  let buffer = Bytes.create buffer_size in
  try
    while true do
      let bytes_read = input file buffer 0 buffer_size in
      if bytes_read = 0 then raise End_of_file;
      ignore (write_substring socket (Bytes.to_string buffer) 0 bytes_read)
    done
  with End_of_file -> close_in file

let rec enter_message () =
  let message = read_line () in
  let some_message =
    match message with
    | "" -> enter_message ()
    | "quit" ->
        quit := true;
        None
    | _ -> Some message
  in
  if not !socket_is_closed then some_message else None

let send_messages socket =
  let _ = socket_is_closed := false in
  let rec send_message_loop () =
    let some_message = enter_message () in
    match some_message with
    | Some message ->
        Mutex.lock mutex;
        let start_time = Unix.gettimeofday () in
        let _ =
          match message with
          | _ when message.[0] = ':' ->
              let file_name =
                String.sub message 1 (String.length message - 1)
              in
              let stats = stat file_name in
              let number_of_messages =
                (stats.Unix.st_size + max_message_size - 1) / max_message_size
              in
              let control_message = ":" ^ string_of_int number_of_messages in
              ignore
                (write socket
                   (Bytes.of_string control_message)
                   0
                   (String.length control_message));
              send_file file_name socket
          | _ ->
              ignore
                (write socket (Bytes.of_string message) 0
                   (String.length message))
        in
        let buffer = Bytes.create max_message_size in
        let _ = read socket buffer 0 max_message_size in
        let end_time = Unix.gettimeofday () in
        let elapsed_time = end_time -. start_time in
        Printf.printf "%.6f second latency\n%!" elapsed_time;
        Mutex.unlock mutex;
        send_message_loop ()
    | _ -> Printf.printf "ending sending \n%!"
  in
  let _ = send_message_loop () in
  close socket

type socket_status = Closed | Empty | Full

let is_socket_closed socket_fd =
  try
    let read_ready, _, _ = Unix.select [ socket_fd ] [] [] 0.0 in
    if read_ready = [] then Empty
    else
      let buffer = Bytes.create 1 in
      let bytes_peeked = Unix.recv socket_fd buffer 0 1 [ Unix.MSG_PEEK ] in
      if bytes_peeked = 0 then Closed else Full
  with _ -> Closed (* Treat unexpected errors as connection closed *)

let receive_file filename socket number_of_messages =
  let file = open_out_bin filename in
  let buffer = Bytes.create max_message_size in
  let rec loop count =
    match count with
    | 0 -> ()
    | _ ->
        let bytes_read = read socket buffer 0 max_message_size in
        output file buffer 0 bytes_read;
        loop (count - 1)
  in
  loop number_of_messages;
  close_out file

let make_file_generator () =
  let counter = ref 0 in
  fun () ->
    let filename = Printf.sprintf "file%d" !counter in
    incr counter;
    filename

let recv_messages socket =
  let _ = quit := false in
  let buffer = Bytes.create max_message_size in
  let next_file = make_file_generator () in
  let rec recv_message_loop () =
    Mutex.lock mutex;
    match (is_socket_closed socket, !quit) with
    | Empty, false ->
        Mutex.unlock mutex;
        recv_message_loop ()
    | Full, false ->
        let bytes_read = read socket buffer 0 max_message_size in
        let message = Bytes.sub_string buffer 0 bytes_read in
        let _ =
          match message with
          | _ when message.[0] = ':' ->
              let number_of_messages =
                int_of_string (String.sub message 1 (String.length message - 1))
              in
              let _ = receive_file (next_file ()) socket number_of_messages in
              Printf.printf "Received a file\n%!"
          | _ -> Printf.printf "Received: \"%s\"\n%!" message
        in
        ignore (* responsed with an ack *)
          (write socket
             (Bytes.of_string ack_message)
             0
             (String.length ack_message));
        Mutex.unlock mutex;
        recv_message_loop ()
    | Closed, _ ->
        Mutex.unlock mutex;
        socket_is_closed := true
    | _, _ -> Mutex.unlock mutex
  in
  recv_message_loop ();
  Printf.printf "done revc\n%!"

let start_server port =
  let socket_address = ADDR_INET (inet_addr_any, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  bind socket socket_address;
  listen socket 1;
  let rec accept_clients_loop () =
    Printf.printf "Waiting connection on port %d\n%!" port;
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

let start_client host port =
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
      | "" -> start_client default_host port
      | _ -> start_client host port)
  | _ -> Printf.printf "not a valid option"
