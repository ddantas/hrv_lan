import cv2
import threading
import socket
import pickle
import struct

class LanCamera:


	def __init__(self, HOST='127.0.0.1', PORT=9000):
	
		self.__running = False
		self.__host = HOST
		self.__port = PORT
		self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

	def list_servers(self):
		pass

	def list_cams_local(self, num):

		# lista as cameras de num ate 0
		index = 0
		v = []
		i = num
		while i > 0:
			cap = cv2.VideoCapture(index)
			if cap.read()[0]:
				v.append(index)
				cap.release()
				
			index += 1
			i -= 1
		return v

	def list_cams_lan(self):
		pass


class Server(LanCamera):

	def __init__(self, HOST='127.0.0.1', PORT=9000):

		self.__running = False
		self.__host = HOST
		self.__port = PORT
		self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.__init_socket()

	def __init_socket(self):
		self.__socket.bind((self.__host, self.__port))

	def start_server(self):

		if self.__running: 
			print("Servidor já está rodando")
		else:
			self.__running = True
			server_thread = threading.Thread(target=self.__server_listen)
			server_thread.start()

	def __server_listen(self):

		self.__socket.listen()
		conn, addr = self.__socket.accept()
		thread = threading.Thread(target=self.__handle_client_conn, args=(conn, addr,))
		thread.start()

	def __handle_client_conn(self, conn, addr, record=False):

		data = b''
		img_data_size = struct.calcsize('>L')
		while self.__running:
			quit = False

			while len(data) < img_data_size:
				recv = conn.recv(4096)
				data += recv
				if data == b'':
					conn.close()
					quit = True
					break
			if quit:
				break

			msg_size = data[:img_data_size]
			data = data[img_data_size:]

			msg_size = struct.unpack(">L", msg_size)[0]

			while len(data) < msg_size:
				data += conn.recv(4096)

			frame_data = data[:msg_size]
			data = data[msg_size:]	
			frame = pickle.loads(frame_data, fix_imports=True, encoding="bytes")
			frame = cv2.imdecode(frame, cv2.IMREAD_COLOR)
			cv2.imshow(str(addr), frame)

			if cv2.waitKey(1) == ord('q'):
				conn.close()
				break


	def stop_server(self):

		if self.__running:
			self.__running = False
			closer = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			closer.connect((self.__host, self.__port))
			closer.close()
			self.__socket.close()

		else:
			print("Não tem servidores rodando")

class Client(LanCamera):

	def __init__(self, HOST='127.0.0.1', PORT=9000, device=0):
	
		self.__running = False
		self.__host = HOST
		self.__port = PORT
		self.__socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.__cam = None
		self.__init_camera(device)

	def __init_camera(self, device):
	
		self.__cam = cv2.VideoCapture(device)

	def __cleanup(self):
		self.__cam.release()
		cv2.destroyAllWindows()

	def start_stream(self, record=False):

		if self.__running:
			print("Cliente já está realizando o streaming")

		else:
			self.__running = True
			client_thread = threading.Thread(target=self.__connection_to_server, args=(self.__host, self.__port, record))
			client_thread.start()

	def __connection_to_server(self, host, port, record):

		self.__socket.connect((host, port))


		if record:
			size = (int(self.__cam.get(3)), int(self.__cam.get(4)))
			vidWriter = cv2.VideoWriter('../media/camera.mp4', cv2.VideoWriter_fourcc(*'MJPG'), 30, size)

		while self.__running:
			ret, frame = self.__cam.read()
			if record:
				vidWriter.write(frame)
			ret, frame = cv2.imencode('.jpg', frame, [int(cv2.IMWRITE_JPEG_QUALITY), 90])
			data = pickle.dumps(frame, 0)
			size = len(data)

			try:
				self.__socket.sendall(struct.pack('>L', size) + data)

			except:
				self.__running = False

		self.__cleanup()

	def stream_stop(self):

		if self.__running:
			self.__running = False
			self.__socket.close()

		else:
			print("O cliente não está realizando o streaming")
