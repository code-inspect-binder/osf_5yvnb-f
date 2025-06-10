////////////////////////////////////////////////////////////////////////////////
//
// __ ______
// / /_______________ ____ ___ / ____/
// / __/ ___/ ___/ __ \/ __ \/ _ \ /___ )
// / /_(__ ) /__/ /_/ / /_/ / __/ ____/ /
// \__/____/\___/\____/ .___/\___/ /_____/
// /_/
//
// serialport_common.c
// - function to set serial port parameters.
// - not part of Tscope5 (because of the huge number of possible parameters
// and the difference between Windows and Posix systems).
////////////////////////////////////////////////////////////////////////////////

#ifndef TS5_WINDOWS

#include <sys/types.h>
#include <sys/ioctl.h>
#include <dirent.h>
#include <termios.h>
#include <errno.h>

#endif

void config_serialport(TS5_SERIALPORT *portptr)
{
	#ifdef TS5_WINDOWS
	
	// SetCommState
	
	DCB dcb;
	
	if (!GetCommState(*portptr, &dcb)) {
		ts5_fatal("%s: GetCommState failed with error code %d\n", GetLastError());
	}
	
	dcb.BaudRate = 9600;
	dcb.ByteSize = (unsigned char) 8;
	dcb.Parity = 0;
	dcb.StopBits = 1;
	dcb.fBinary = 1;
	
	if (!SetCommState(*portptr, &dcb)) {
		ts5_fatal("%s: SetCommState failed with error code %d\n", GetLastError());
	}
	
	// SetCommTimeouts
	
	COMMTIMEOUTS ct;
	ct.ReadIntervalTimeout = MAXDWORD;
	ct.ReadTotalTimeoutMultiplier = 0;
	ct.ReadTotalTimeoutConstant = 0;
	ct.WriteTotalTimeoutMultiplier = 0;
	ct.WriteTotalTimeoutConstant = 5000;
	
	if (!SetCommTimeouts(*portptr, &ct)) {
		ts5_fatal("%s: SetCommTimeouts failed with error code %d\n", GetLastError());
	}
	
	// SetupComm
		if (!SetupComm(*portptr, 2048, 2048)) {
		ts5_fatal("%s: SetupComm failed with error code %d\n", GetLastError());
	}
	
	#else
	
	// set options for the connection
	
	struct termios options, oldoptions;
	tcgetattr(*portptr,
	&oldoptions);
	bzero (&options, sizeof (options));
	cfsetispeed(&options, B9600);
	cfsetospeed(&options, B9600);
	options.c_iflag = IGNBRK;
	options.c_lflag = 0;
	options.c_oflag = 0;
	options.c_cflag |= CLOCAL | CREAD;
	options.c_iflag = IGNPAR;
	options.c_oflag = 0;
	options.c_cflag &= ~(PARENB|PARODD) ;
	options.c_cflag &= ~CSTOPB ;
	options.c_cflag &= ~CSIZE;
	options.c_cflag |= CS8;
	options.c_cflag &= ~CRTSCTS;
	options.c_iflag &= ~(IXON | IXOFF | IXANY);
	
	if (tcsetattr (*portptr, TCSANOW, &options) == -1) {
		ts5_fatal("config_serialport: could not set options on usb serial device\n");
	}
	if (fcntl(*portptr, F_SETFL, FNDELAY) == -1) {
		ts5_fatal("config_serialport: could not set delay on usb serial device\n");
	}
	
	#endif
}
