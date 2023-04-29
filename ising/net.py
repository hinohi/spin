import sys

template = """
module net_mod
    implicit none
    character(3) :: net_type="{net_type}"
    integer, parameter :: N={N}, D={D}
    integer :: net(D, N) = reshape((/ &
{net}
        /), shape(net))
end module net_mod
"""

def make_net_sq2(L):
    if L < 3:
        raise ValueError("L must be greater than 2(L=%i)"%L)
    N = L**2
    D = 4
    net = []
    for y in xrange(L):
        for x in xrange(L):
            l = []
            for dx, dy in [1,0], [-1,0], [0,1], [0,-1]:
                l.append((y+dy)%L*L + (x+dx)%L)
            net.append(l)
    return N, D, net

def make_net_sq3(L):
    if L < 3:
        raise ValueError("L must be greater than 2(L=%i)"%L)
    N = L**3
    D = 6
    net = []
    for z in xrange(L):
        for y in xrange(L):
            for x in xrange(L):
                l = []
                for dx,dy,dz in [1,0,0], [-1,0,0], [0,1,0], [0,-1,0], [0,0,1],[0,0,-1]:
                    l.append((z+dz)%L*L*L + (y+dy)%L*L + (x+dx)%L)
                net.append(l)
    return N, D, net

def make_net_tri(L):
    if L < 3:
        raise ValueError("L must be greater than 2(L=%i)"%L)
    N = L**2
    D = 6
    net = []
    for y in xrange(L):
        for x in xrange(L):
            l = []
            for dx,dy in [1,0],[-1,0],[0,1],[0,-1],[1,1],[-1,-1]:
                l.append((y+dy)%L*L + (x+dx)%L)
            net.append(l)
    return N, D, net

def make_net_hls(L):
    if L < 3 or L%3 != 0:
        raise ValueError("L must be 3,6,9...(L=%i)"%L)
    LL = L * 2 / 3
    N = L * LL
    D = 3

    m = []
    i = 0
    for y in xrange(L):
        l = []
        for x in xrange(L):
            if (x+y)%3 == 2:
                l.append(-1)
            else:
                l.append(i)
                i += 1
        m.append(l)

    net = []
    for y in xrange(L):
        for x in xrange(L):
            a = (x+y)%3
            if a == 2: continue
            l = []
            if a == 0:
                for dx,dy in [1,0],[0,1],[-1,-1]:
                    l.append(m[(y+dy)%L][(x+dx)%L])
            else:
                for dx,dy in [1,1],[-1,0],[0,-1]:
                    l.append(m[(y+dy)%L][(x+dx)%L])
            net.append(l)
    return N, D, net

def parse_arg():
    net_type = sys.argv[1]
    L = int(sys.argv[2])
    return net_type, L

def format_net(net):
    s = []
    for l in net[:-1]:
        s.append("      & " + ", ".join([str(j+1)for j in l]) + ", &\n")
    s.append("      & " + ", ".join([str(j+1)for j in net[-1]]) + " &")
    return "".join(s)

def main():
    net_type, L = parse_arg()
    try:
        N, D, net = eval("make_net_%s(%i)"%(net_type, L))
    except NameError:
        raise NameError("network type '%s' is not defined"%net_type)

    s = template.format(net_type=net_type,
                        N=N,
                        D=D,
                        net=format_net(net))
    open("net_mod.f90", "w").write(s)

if __name__ == '__main__':
    main()
