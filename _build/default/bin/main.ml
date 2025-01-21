open Unix

let send_messages socket_fd =
  while true do
    let message = read_line () in
    let start_time = gettimeofday () in
    ignore (write socket_fd (Bytes.of_string message) 0 (String.length message));
    let buffer = Bytes.create 1024 in
    let _ = read socket_fd buffer 0 1024 in
    let end_time = gettimeofday () in
    let elapsed_time = end_time -. start_time in
    Printf.printf "Server response: %.6f seconds\n%!" elapsed_time
  done

let recv_messages client_fd =
  let buffer = Bytes.create 1024 in
  while true do
    let bytes_read = read client_fd buffer 0 1024 in
    Printf.printf "Received: %s\n%!" (Bytes.sub_string buffer 0 bytes_read);
    let response = "ack" in
    ignore
      (write client_fd (Bytes.of_string response) 0 (String.length response))
  done

let start_server port =
  let sockaddr = ADDR_INET (inet_addr_any, port) in
  let socket_fd = socket PF_INET SOCK_STREAM 0 in
  bind socket_fd sockaddr;
  listen socket_fd 1;
  Printf.printf "Server listening on port %d\n%!" port;
  while true do
    let client_fd, client_addr = accept socket_fd in
    Printf.printf "Client connected: %s\n%!"
      (match client_addr with
      | ADDR_INET (addr, _) -> string_of_inet_addr addr
      | _ -> "Unknown");
    recv_messages client_fd
  done

let connect_to_server host port =
  let sockaddr = ADDR_INET (inet_addr_of_string host, port) in
  let socket_fd = socket PF_INET SOCK_STREAM 0 in
  connect socket_fd sockaddr;
  Printf.printf "Connected to server %s:%d\n%!" host port;
  send_messages socket_fd

let () =
  let host = "127.0.0.1" in
  let port = 8080 in
  print_string "Do you want to be a server or client? (s/c)";
  let server_option = read_line () in
  match server_option with
  | "s" -> start_server port
  | "c" -> connect_to_server host port
  | _ -> Printf.printf "not a valid option"
