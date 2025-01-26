# Ocaml Chat
## Running
Clone the repo and run the following one of the following:
### Build with Dune
```dune exec ocaml_chat_socket``` 
### Compile with ocamlc
```ocamlc -thread unix.cma threads.cma -o prog main.ml```\
```./prog```

## Select Mode 
Select a mode with ```s``` or ```c```.
If client was selected enter the IP to connect to.

## Sending Messages
 - Just type what you want to say and press enter
 - Start a message with ```:``` followed by a file name to send a file (dont put anything after the file name)
 - Type "quit" to disconnect

[Screencast from 2025-01-25 18-55-13.webm](https://github.com/user-attachments/assets/16c6156d-cce4-4499-9a9b-269c14eafc66)
