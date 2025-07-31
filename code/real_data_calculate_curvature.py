## CALCULATE CURVATURES ##
import networkx as nx
from scipy.io import loadmat
import sys
sys.path.insert(1, "/code/curvatures/prev_curv")
from balanced_forman_curvature import BalancedForman
from forman_ricci_curvature import FormanRicci
from lower_ricci_curvature import LowerORicci

def balancedcurv(G: nx.Graph):
    bfc = BalancedForman(G)
    G = bfc.compute_balancedformancurv()
    return G

def formancurv(G: nx.Graph):
    frc = FormanRicci(G)
    G = frc.forman_curvature()
    return G

def lowercurv(G: nx.Graph):
    lrc = LowerORicci(G)
    G = lrc.compute_lower_curvature()
    return G

    
if __name__ == '__main__':
    mat = loadmat('/data/caltech.mat')
    adj = mat['A']
    G = nx.from_numpy_array(adj)
    G = balancedcurv(G)
    print("BFC DONE")
    G = formancurv(G)
    print("FRC DONE")
    G = lowercurv(G)
    print("LRC DONE")
    nx.write_gml(G, "/data/caltech_curvs.gml")
    print("Graph saved")