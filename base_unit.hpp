#ifndef BASE_UNIT_HPP
#define BASE_UNIT_HPP
#include "base_unit_traits.hpp"

/* 
 * This file contains the definitions of base_units, 
 * which are the objects we work with. 
 * */

namespace unit {

    /*
    * base_unit is a template class which represents our objects 
    * that use the "unit" API. It consists of a public accessable 
    * static field named degree, whic is initialized via the forwarded 
    * template argument N. It stores the power (degree) of an unit.
    * */

    template <int N>
    class base_unit {

    public:
        static constexpr int degree = N;

    };

    /* 
    * meter<int N> is a template class which represents the unit meter.
    * It inherits from the tempalte class base_unit. It implements
    * the static method str(), which returns the string representation 
    * of the unit second and it has a inner_type filed, which 
    * is also an alias template representing the meter type (the 
    * type generating template) from which we got the meter<int N>
    * type. Its template argument is a integer which initializes 
    * the degree field.
    * */
    template <int N>
    class meter : public base_unit<N>
    {
    public:
        static const char* str() 
        {
            return "m";
        }
        
        template <int M>
        using inner_type = meter<M>;
    };


    /* 
    * second<int N> is a template class which represents the unit second.
    * It inherits from the tempalte class base_unit. It implements
    * the static method str(), which returns the string representation 
    * of the unit second and it has a inner_type filed, which 
    * is also an alias template representing the second type (the 
    * type generating template) from which we got the second<int N>
    * type. Its template argument is a integer which initializes 
    * the degree field.
    * */
    template <int N>
    class second : public base_unit<N>
    {
    public:
        static const char* str()
        {
            return "s";
        }

        template <int M>
        using inner_type = second<M>;
    };

}

#endif