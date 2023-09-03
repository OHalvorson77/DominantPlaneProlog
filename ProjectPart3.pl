%Owen Halvorson 0300251644


read_xyz_file(File, Points) :-
 open(File, read, Stream),
read_xyz_points(Stream,Points),
 close(Stream).
read_xyz_points(Stream, []) :-
 at_end_of_stream(Stream).
read_xyz_points(Stream, [Point|Points]) :-
 \+ at_end_of_stream(Stream),
read_line_to_string(Stream,L), split_string(L, "\t", "\s\t\n",
XYZ), convert_to_float(XYZ,Point),
 read_xyz_points(Stream, Points).
convert_to_float([],[]).
convert_to_float([H|T],[HH|TT]) :-
 atom_number(H, HH),
 convert_to_float(T,TT).

%Predicate generates 3 random points from the list of points
% Parameters: Points(A list of points of [X,Y,Z] format), a triplet of
% points(a triplet of randomly generated points of [X,Y,Z] format.

random3points(Points, [[X1, Y1, Z1], [X2, Y2, Z2], [X3, Y3, Z3]]) :-
    length(Points, N),%Calculates the length of the Points list and stores it in N
    N >= 3,%Makes sure N is at least 3
    random_permutation(Points, [Pt1, Pt2, Pt3 | _]),%Selects 3 random points from points
    Pt1 = [X1, Y1, Z1],%Stores the three random points in [[X1,Y1,Z1],.....]
    Pt2 = [X2, Y2, Z2],
    Pt3 = [X3, Y3, Z3].


% Plane returns true if it is the proper [A,B,C,D] representation of the
% plane given 3 points
%
% Parameters:(3 [X,Y,Z] lists used to represent 3 points, have to
% calcualte plane from them), Plane(Represented as a [A,B,C,D] list, is
% the proper plane equation of the 3 points.

plane([[X1, Y1, Z1], [X2, Y2, Z2], [X3, Y3, Z3]], Plane) :-
    A is Y1*(Z2-Z3) + Y2*(Z3-Z1) + Y3*(Z1-Z2),%Calculating A value
    B is Z1*(X2-X3) + Z2*(X3-X1) + Z3*(X1-X2),%Calculating B value
    C is X1*(Y2-Y3) + X2*(Y3-Y1) + X3*(Y1-Y2),%Calculating C value
    D is -X1*(Y2*Z3-Y3*Z2) - X2*(Y3*Z1-Y1*Z3) - X3*(Y1*Z2-Y2*Z1),%Isolating for D value
    Plane = [A, B, C, D].%Setting plane to be a list of A,B,C,D

% Distance predicate returns true if Distance is the distance between
% plane [A,B,C,D] and Point [X,Y,Z]
% Parameters: [A,B,C,D](integer Values used to represent a plane),
% [X,Y,Z](Integer values used to represent a point), Distance(decimal
% value that is the distance from point to plane).

distance([A,B,C,D],[X,Y,Z], Distance):-
 Top is abs(A*X + B*Y + C*Z +D),%Calculating the top of the distance equation
 Bottom is sqrt(A^2 +B^2 +C^2),%Calculating the bottom of the distance equation
 Distance is Top/Bottom.%Dividing top of equation by bottom of equation to get Distance


% Returns true if N is the proper support for the gien plane and points
% using eps distance
% Parameters: Plane(List that holds A B C D values, used to show the
% plan), points(List of points, each with x,y,z values), eps (Decimal
% value, maximum distance away from plane point is allowed for it to
% count in the support of plane), N(Integer value that is support of
% plane).

support(Plane, Points, Eps, N):-
 findall(P, (member(P, Points), distance(Plane, P, D), D =< Eps), Candidates), %Finding all points within the Eps distance from plane and storing in                                                                                  candidates
 length(Candidates, N).%Finding the length of the list of candidates and making that the Support


% Ransac-Number-of-iterations tests if N is the correct number of
% iterations needed for a given confidence level and percentage
% of points of plane
% Parameters: Confidence(decimal value thats the confidence we want for
% dominant plane), Percentage(Decimal value for percentage of points on
% plane, N(Integer for number of iterations with confidence and
% percentage).

ransac-number-of-iterations(Confidence, Percentage, N) :-
    N is ceiling(log(1 - Confidence) / log(1 - Percentage^3)).%Using number of iterations formula to get N


%TEST CASES

%Test cases for random3points
test(random3points, 1) :-%Tests if random 3 points generates 3 distinct points from the point list
    Points = [[1,2,3], [4,5,6], [7,8,9], [10,11,12]],
    random3points(Points, [[X1,Y1,Z1], [X2,Y2,Z2], [X3,Y3,Z3]]),
    \+ [X1,Y1,Z1] = [X2,Y2,Z2],
    \+ [X1,Y1,Z1] = [X3,Y3,Z3],
    \+ [X2,Y2,Z2] = [X3,Y3,Z3].
test(random3points, 2):-%Tests if the points generated are within the range of the point list
 Points = [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15]],
random3points(Points, [[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]]),
(X1>=1),(X1=<13),
(Y1>=2),(Y1=<14),
(Z1>=3),(Z1=<15),
(X2>=1),(X2=<13),
(Y2>=2),(Y2=<14),
(Z2>=3),(Z2=<15),
(X3>=1),(X3=<13), (Y3>=2),(Y3=<14),
(Z3>=3),(Z3=<15).

%3 Test cases for the plane predicate
test(plane, 1):- plane([[1,0,0],[0,1,0],[0,0,1]], [1,1,1,-1]).
test(plane, 2):- plane([[0,0,0], [1,1,1], [2,2,2]], [1, -1, 0, 0]).
test(plane, 3):- plane([[1,2,3],[4,5,6],[7,8,9]], [0,0,0,0]).

%4 Test cases for ransac-number-of-iterations predicate
test(ransac-number-of-iterations,1):- ransac-number-of-iterations(0.99, 0.12, 2663).
test(ransac-number-of-iterations,2):- ransac-number-of-iterations(0.95, 0.1, 100).
test(ransac-number-of-iterations,3):- ransac-number-of-iterations(0.999, 0.8, 10).
test(ransac-number-of-iterations,4):- ransac-number-of-iterations(0.998, 0.15, 1839).

%4 Test cases for the support predicate
test(support, 1):-support([5,2,1,3],[[6,1,2],[7,7,7],[6,0,2]],7,2).
test(support, 2) :- support([1,0,0,0], [[1,0,0],[0,1,0],[0,0,1],[-1,0,0]], 0.5, 2).
test(support, 3) :- support([4,5,4,3],[],1, 0).
test(support, 4) :- support([1,3,5,3], [[1,4,5],[3,4,2],[4,3,8],[5,3,1],[-1,-2,3],[-4,-5,-6],[3,2,1],[1,2,3]],5,3).







