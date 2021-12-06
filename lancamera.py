import cv2
import threading
import socket
import pickle
import struct
from scapy.all import *
import time

class LanCamera:


    def __init__(self, HOST='127.0.0.1', PORT=9000):
    
        self.__running = False
        self.__host = HOST
        self.__port = PORT
        self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)


    def list_servers(self, port_range: list):
    # Lists all available servers on LAN that have certain ports open
    # The opened ports should belong to the interval 'port_range'

        hosts = self.__scan_ports(port_range)
        if len(hosts) > 0:
            print("Servidores encontrados: ")
            for host, ports in hosts.items():
                print(f"IP Servidor: {host}; Portas abertas: {ports}")
        else:
            print("Nenhum servidor foi encontrado")

        return hosts


    def __scan_ports(self, port_range: list):
    # For each ip obtained through __get_ips(), determines which port, in 'port_range' is opened

        if type(port_range) is int:
            start = port_range
            end = start+1
        else:
            start = port_range[0]
            end  = port_range[1]

        ips = self.__get_ips()
        ips.append("127.0.0.1")
        hosts = {}
        for ip in ips:
            ports = []
            print(f"Scaneando portas {start}-{end-1} do host {ip}")
            for port in range(start, end):
                if self.__scan_port(ip, port):
                    ports.append(port)

            if len(ports) > 0:
                hosts[ip] = ports

        return hosts

    def __scan_port(self, ip, port, timeout=2):
    # Tries to connect to a specific port and IP to see if the port is opened

        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(timeout)

        if s.connect_ex((ip, port)) == 0:
            s.close()
            return True

        s.close()
        return False


    def __get_ips(self, network='192.168.1.0/24'):
    # Pings via ARP protocol all hosts in the network to obtain the IPs of available hosts in the network
    # (MUST RUN AS ROOT TO PING VIA ARP)

        ans,unans = srp(Ether(dst="ff:ff:ff:ff:ff:ff")/ARP(pdst=network),timeout=2)
        ips = []
        for res in ans.res:
            ips.append(res.answer.psrc)

        return ips

    def list_cams_local(self, num):
    # Lists all the available capture devices with index up to 'num' in localhost 

        index = 0
        v = []
        i = num
        while i > 0:
            cap = cv2.VideoCapture(index)
            if cap.isOpened():
                v.append(index)
                cap.release()
                
            index += 1
            i -= 1
        return v


class Client(LanCamera):

    def __init__(self, HOST='127.0.0.1', PORT=9000):

        self.__running = False
        self.__connected = False
        self.__host = HOST
        self.__port = PORT
        self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.__socket_commands = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def list_cams_lan(self):
    # List cameras from available hosts in the network

        hosts = self.list_servers(9001)
        for ip, ports in hosts.items():
            for port in ports:
                self.__socket_commands.connect((ip, port))
                self.__socket_commands.sendall(b"LIST")
                cams = self.__socket_commands.recv(1024).decode()
                print(f'Cameras do servidor {ip}:{port} -> {cams}')
                self.__socket_commands.close()

    def start_connection(self, record=False):
    # Starts a thread that does the connection with the streaming server
    # If record=True, __handle_conn will save the frames to the hard drive

        if self.__running: 
            print("O cliente já está rodando")
        else:
            self.__running = True
            server_thread = threading.Thread(target=self.__connect, args=(record, ))
            server_thread.start()

    def __connect(self, record):
    # Connects to the streaming server and starts a thread the handles the streaming data that is received
    # If record=True, __handle_conn will save the frames to the hard drive

        self.__socket.connect((self.__host, self.__port))
        self.__connected = True
        thread = threading.Thread(target=self.__handle_conn, args=(record, ))
        thread.start()


    def __handle_conn(self, record):
    # Receives, decodes and display the streaming frames that were received from the server
    # If record=True, it saves the frames to the hard drive

        if record:
            cod = cv2.VideoWriter_fourcc(*'MJPG')
            vidWriter = cv2.VideoWriter('media/camera_client.mp4',cod,30.0,(640,480))

        data = b''
        img_data_size = struct.calcsize('>L')
        while self.__running:
            quit = False

            while len(data) < img_data_size:
                recv = self.__socket.recv(4096)
                data += recv
                if data == b'':
                    self.__socket.close()
                    quit = True
                    break
            if quit:
                break

            msg_size = data[:img_data_size]
            data = data[img_data_size:]

            msg_size = struct.unpack(">L", msg_size)[0]

            while len(data) < msg_size:
                data += self.__socket.recv(4096)
                if data == b'':
                    # self.__socket.close()
                    self.stop_client()
                    quit = True
                    break
            if quit:
                break 
                
            frame_data = data[:msg_size]
            data = data[msg_size:]  
            frame = pickle.loads(frame_data, fix_imports=True, encoding="bytes")
            frame = cv2.imdecode(frame, cv2.IMREAD_COLOR)

            if record:
                vidWriter.write(frame)

            cv2.imshow(str(self.__host), frame)

            if cv2.waitKey(1) == ord('q'):
                self.stop_client()
                break

    def stop_client(self):
    # Stops the client connection to the streaming server

        if self.__running:
            self.__running = False
            self.__socket.close()

        else:
            print("Não tem clientes rodando")

class Server(LanCamera):

    def __init__(self, HOST='127.0.0.1', PORT=9000, device=0):
    
        self.__running = False
        self.__streaming = False
        self.__host = HOST
        self.__port = PORT
        self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.__socket_commands = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.__cam = None
        self.__init_camera(device)
        self.__init_socket()

    def __init_socket(self):
    # Initializes all sockets binding them to specific ports
        self.__socket.bind((self.__host, self.__port))
        self.__socket_commands.bind((self.__host, self.__port+1))

    def __init_camera(self, device):
    # Initializes video capture device
        self.__cam = cv2.VideoCapture(device)

    def __cleanup(self):
    # Destroy video capture device and window data structures
        self.__cam.release()
        cv2.destroyAllWindows()

    def start_commands(self):
    # Starts a thread to receive commands from the client

        if self.__running:
            print("O servidor já está rodando")

        else:
            self.__running = True
            thread = threading.Thread(target=self.__server_listen_commands)
            thread.start()

    def __server_listen_commands(self):
    # Accepts connections coming to the port that handles commands and starts a thread to 
    # handle incoming commands

        while self.__running:
            self.__socket_commands.listen()
            conn, addr = self.__socket_commands.accept()
            thread = threading.Thread(target=self.__handle_commands, args=(conn, ))
            thread.start()

    def __handle_commands(self, conn):
    # Handles the data received, which should be a command specifying an action
    # (Only LIST is implemented)

        while self.__running:
            data = conn.recv(1024)

            if data == b"LIST":
                cams = self.list_cams_local(5)

                try:
                    print(cams)
                    conn.sendall(str(cams).encode())
                except:
                    conn.close()

            if data == b'':
                # self.__running = False
                conn.close()
                break

    def stop_commands_server(self):
    # Stops the server that is waiting for connections to execute commands

        if self.__running:
            self.__running = False
            closer = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            closer.connect((self.__host, self.__port+1))
            closer.close()
            self.__socket_commands.close()

        else:
            print("O servidor não está realizando o streaming")
  
    def start_stream(self):
    # Starts a thread to listen for connections for video streaming

        if self.__streaming:
            print("O servidor já está realizando o streaming")

        else:
            self.__streaming = True
            server_thread = threading.Thread(target=self.__server_listen, args=(record, ))
            server_thread.start()

    def __server_listen(self):
    # Listens for an incoming connection and starts a thread that handles the sending of 
    # camera frames after the connection is established

        self.__socket.listen()
        conn, addr = self.__socket.accept()
        thread = threading.Thread(target=self.__stream, args=(conn, record, ))
        thread.start()

    def __stream(self, conn):
    # Captures and encodes camera frames in order to send them via the streaming socket

        while self.__streaming:
            ret, frame = self.__cam.read()
            ret, frame = cv2.imencode('.jpg', frame, [int(cv2.IMWRITE_JPEG_QUALITY), 90])
            data = pickle.dumps(frame, 0)
            size = len(data)

            try:
                conn.sendall(struct.pack('>L', size) + data)

            except:
                self.__streaming = False

        self.__cleanup()

    def stop_stream(self):
    # Stops and closes the streaming server

        if self.__streaming:
            self.__streaming = False
            closer = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            closer.connect((self.__host, self.__port))
            closer.close()
            self.__socket.close()

        else:
            print("O servidor não está realizando o streaming")