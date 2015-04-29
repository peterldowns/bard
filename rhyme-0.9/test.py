#!/usr/bin/python

import os,sys,string,re,time

ExitError = "error executing process"

multiple = re.compile(".*\(.*\)")

def run(command, args):
    pid = os.fork()
    if (pid == 0):
        #redirect standard output to /dev/null at a low-level
        devnull = os.open("/dev/null", os.O_WRONLY | os.O_APPEND)
        os.dup2(devnull, 1)

        #then execute the process
        os.execvp(command, list([command] + list(args)))
        raise ExitError  #or raise an error
    else:
        status = os.wait()[1]

        if (os.WIFEXITED(status)):
            return os.WEXITSTATUS(status)
        else:
            raise ExitError

if (__name__ == '__main__'):
    #print(run("./rhyme", ["g54jg8954g"]))

    print("Checking the build ... this will take a very VERY long time")
    print("(feel free to kill this process if you have better things to do)")

    time.sleep(5)

    line = sys.stdin.readline()

    while (line):
        line = string.lower(string.split(string.strip(line), " ")[0])
        
        if (not multiple.match(line)):
            print("Testing \"%s\"" % (line))
            if (run("./rhyme", [line]) != 0):
                print("Error searching for \"%s\"" % (line))
                break

        line = sys.stdin.readline()
    else:
        print("All tests completed successfully!")
