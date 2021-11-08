#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

import sys
import do

def _help (file=sys.stdout):
    print("USAGE: `%s [ACTION]... [DATA]...'" % sys.argv[0], file=file)
    print("ACTIONS: --bar, --add, --remove, --list, --todo, --help", file=file)


class Task (object):
    def __init__ (self, name, description, is_done):
        self.name = name
        self.description = description
        self.is_done = is_done


def _get_data ():
    file = open("todo.data", 'r')
    data = file.read().split('\n')
    file.close()
    del file;

    todo_data = {}
    for line in data:
        try:
            name, description, is_done = line.split('\t')
        except:
            continue;
        else:
            todo_data.update({name:
                              Task(name, description, is_done=="[DONE]")})

    return todo_data;


def _dump_data (data):
    file = open("todo.data", "w")
    for name in data:
        print("%s\t%s\t%s" % (data[name].name,
                              data[name].description,
                              "[DONE]" if data[name].is_done else "[TODO]"),
              file=file)
    file.close()


def _error (text, exit=-1):
    print("\033[31;1mERROR\033[0m: \033[2m%s\033[0m" % text, file=sys.stderr)
    sys.exit(exit)


def _get_task (todo_dict, data):
    try:
        int(data)
    except ValueError:
        if data not in todo_dict.keys():
            _error("%s is not a valid task name!" % data, 4)
            return;
    else:
        data = int(data)
        if data >= len(todo_dict):
            _error("%d is out of bounds!" % data, 3)
            return;
        data = list(todo_dict.keys())[data]

    return data


fields = {
        "all"     : lambda name : True,
        "bugs"    : lambda name : "_bug"     in name or "bug_" in name,
        "ada"     : lambda name : "_ada"     in name or "ada_" in name,
        "c"       : lambda name : "_c"       in name or "c_"   in name,
        "py"      : lambda name : "_py"      in name or "py_"  in name,
        "doc"     : lambda name : "_doc"     in name or "doc_" in name,
        "sys"     : lambda name : "_sys"     in name or "sys_" in name,
        "tests"   : lambda name : "tests"    in name,
        "cleanup" : lambda name : "cleanup"  in name,
        "colors"  : lambda name : "colors"   in name,
        "surfaces": lambda name : "surfaces" in name,
}


def main ():
    BAR    = 0
    ADD    = 1
    REMOVE = 2
    LIST   = 3
    TODO   = 4
    DO     = 5
    UNDO   = 6
    
    data = []
    action = [BAR]
    for arg in sys.argv[1:]:
        if arg == "--bar" or arg == "-b":
            action.append(BAR)
        elif arg == "--add" or arg == "-a":
            action = [ADD]
        elif arg == "--remove" or arg == "-r":
            action = [REMOVE]
        elif arg == "--list" or arg == "-l":
            action.append(LIST)
        elif arg == "--todo" or arg == "-t":
            action.append(TODO)
        elif arg == "--do" or arg == "-d":
            action = [DO]
        elif arg == "--undo" or arg == "-u":
            action = [UNDO]
        elif arg == "--help" or arg == "-h":
            _help()
            return
        elif arg[:2] == "--" or arg[:1] == "-":
            _help(sys.stderr)
            sys.exit(2);
        else:
            data.append(arg)

    user_data = data
    todo_data = _get_data ()
    if ADD in action:
        for name, description in [(user_data[n], user_data[n+1])
                                  for n in range(0, len(user_data), 2)]:
            if name not in todo_data:
                todo_data.update({name: Task(name, description, False)})

    elif REMOVE in action:
        for name in user_data:
            name = _get_task(todo_data, name)
            todo_data.pop(name)

    elif DO in action:
        for name in user_data:
            name = _get_task(todo_data, name)
            todo_data[name].is_done = True

    elif UNDO in action:
        for name in user_data:
            name = _get_task(todo_data, name)
            todo_data[name].is_done = False

    
    if TODO in action:
        do._todo()
        print('\n')

    if LIST in action:
        n = 0
        for dat in todo_data:
            print("%d. %s\t\033[33m%s\033[0m:\n\t\033[2m%s\033[0m" % (
                                     n,
                    "\033[32m[DONE]" if todo_data[dat].is_done else
                    "\033[34m[BUGS]" if fields["bugs"](dat)    else
                    "\033[31m[TODO]",
                                     todo_data[dat].name,
                                     todo_data[dat].description))
            print()
            n += 1

    def _get_perc (f : float) -> str:
        s = "%.2f" % (f*100)
        s = (6 - len(s))*' ' + s
        return s

    if BAR in action:
        for field in fields:
            count = [0, 0]
            check = fields[field]
            for dat in todo_data:
                if check(dat):
                    count[1] += 1
                    count[0] += 1 if todo_data[dat].is_done else 0
            if count == [0, 0]:
                count = [1, 1]
            length = 40
            string = "["
            colour = ["\033[31m", "\033[33m", "\033[32m", "\033[35;1m"]
            perc = count[0] / count[1]
            string += colour[int(perc * 3)]
            n = int(perc * length)
            string += "#" * n
            string += "\033[0m" + " " * (length - n)
            string += "] %s%s%%\033[0m  \033[34m%s\033[0m" % \
                            (colour[int(perc * 3)], _get_perc(perc), field)
            
            print(string)

    _dump_data (todo_data)


if __name__ == "__main__":
    main ()
