# Compound-Unit

This is a project for the course "Programming paradigms".<br>
Paradigm: functional, generic;<br>
Language: C++;<br>
<br>
The general idea for the project:<br>
We want to define a generic type "compound_unit" which would be used for the representation of different measurment units. <br>
For example: <code>compound_unit<meter<1>, second<-2>></code> would represent m/s^2.<br>
When we would multiply values, for example:<br>
<code>32m/s^2 * 5s</code> <br>
we should get a value with the type m/s, namely <code><meter<1>, second<-1>></code>.<br>
<br><br>
The program is compiled using the following command:<br>
<code>g++ -std=c++2a -o demo demo.cpp</code>, <br>
and you run the program with:<br>
<code>./demo</code>.<br>

# Team members
  
Maja Cekić <a href = https://github.com/majacekic> maja-cekic </a>

Aleksandar Nikolić <a href = https://github.com/Gillgalard> aleksandar-nikolic </a>
  
