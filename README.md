# GenFunCpp

This is a project for the course "Programming paradigms".<br>
Paradigm: functional, generic;<br>
Language: C++;<br>
The general idea for the project:<br>
<br>
We want to define a generic type "compound_unit" which would be used for the representation of different measurment units. <br>
For example: <code>compound_unit<meter<1>, second<-2>></code> would represent m/s^2.<br>
When we would multiply values, for example:<br>
<code>32m/s^2 * 5s</code> <br>
we should get a value with the type m/s, namely <code><meter<1>, second<-1>></code>.<br>
