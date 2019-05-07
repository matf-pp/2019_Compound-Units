#ifndef COMPOUND_UNIT_HPP
#define COMPOUND_UNIT_HPP
#include "base_unit.hpp"
#include <type_traits>
#include <string>

/*
 * This file contain the definition and the algorithms of compound_unit.
 * */

namespace unit 
{

    /* Definition of compound unit
     * It is defines as a simple variadic class template.
     * */
    template <typename... Elements>
    class compound_unit
    {};

    /* is_empty <compound_unit>
    *  is meta function that checks if the compound unit is empty.
    *  It consists of two blocks, one for the recursive case and
    *  one for the base case, when CU has no elements in it 
    *  (it is a template specialization for compound_unit<>).
    *  In the first block it evaluates false, and in the second 
    *  it evaluates true;
    *  */
    template <typename CU>
    class is_empty_cu
    {
    public:
        static constexpr bool value = false;
    };

    template <>
    class is_empty_cu<compound_unit<>> 
    {
    public:
        static constexpr bool value = true;
    };


    /* 
    * front_t <compound_unit>
    * is a meta function that has a compount_unit as its template
    * argument and returns the first element of the compound_unit.
    * The second template is an alias template that wrapes the first
    * one.
    * */
    template <typename CU>
    class front;

    template <typename Head, typename... Tail>
    class front<compound_unit<Head, Tail...>>
    {
    
    public:
        using type = Head;
    };

    template <typename CU>
    using front_t  = typename front<CU>::type;


    /*
    * pop_front_t <compound_unit>
    * is a meta function that has a compound_unit as its template 
    * argument and returns the compound_unit without the first element.
    * The second template is an alias template that wrapes the first
    * one.
    * */
    template <typename  CU>
    class pop_front;

    template <typename Head, typename... Tail>
    class pop_front<compound_unit<Head, Tail...>> 
    {
    public:
        using type = compound_unit<Tail...>;
    };

    template <typename CU>
    using pop_front_t = typename pop_front<CU>::type;

    
    /* 
    * push_front_t <compound_unit, new_element>
    * is a meta function that has a compound_unit and a new_element
    * (that is a type that satisfies the compoun_unit interface) as
    * template arguments. It returns a compound unit whose first element
    * is the new_element and the tail is the compound_unit.
    * The second template is a wraper for the first one.
    * */

    template <typename CU, typename NewElement>
    class push_front;

    template <typename... Elements, typename NewElement>
    class push_front<compound_unit<Elements...>, NewElement> 
    {
    public: 
        using type = compound_unit<NewElement, Elements...>;
    };

    template <typename CU, typename NewElement>
    using push_front_t = typename push_front<CU, NewElement>::type;

    /* 
    * cu_interface_satisfies_all <compound_unit>
    * is a meta function that checks if all elements 
    * of the compount_unit (which we got as the template
    * argument) satisfie the needed condition so that the 
    * "compound_unit" API can be used. It consists of a basic
    * case (when the compound_unit is empty) and recursive case
    * (otherwise).
    * */ 

    template <typename CU,
              bool = is_empty_cu<CU>::value>
    class cu_interface_satisfies_all;

    template <typename CU>
    class cu_interface_satisfies_all<CU, false>
    {
    
    public: 
        static constexpr bool value = interface_satisfies_all_v<front_t<CU>> &&
                                      cu_interface_satisfies_all<pop_front_t<CU>>::value;
    };

    template <typename CU>
    class cu_interface_satisfies_all<CU, true>
    {
    public: 
        static constexpr bool value = true;
    };

    /* i_tndexing <compound_unit, int>
    * is a meta function that has a compound_unit as its template
    * argument and a integer, and returns the type on the nth position
    * in the compound_unit. It consists of a recursive case (when the 
    * compound_unit is not empty) and a base case (otherwise).
    * */

    template <typename CU, unsigned N>
    class indexing : public indexing<pop_front_t<CU>, N-1>
    {
    private:
        static_assert(cu_interface_satisfies_all<CU>::value, "CU consists of invalid types for this interface");
    };

    template <typename CU>
    class indexing<CU, 0> : public front_t<CU>
    {

    };

    /* 
    * to_str_type_t <compound_unit>
    * is a meta function that has a compound_unit as its 
    * tempalte argument and produces a type that has a str()
    * method which is recursevly called for every sub-unit of the
    * compound_unit. The function in calls the str() method of every 
    * type and constructs a std::string representation of the 
    * compound_unit. This function is used for debuging exclusevly,
    * since the output code is large. 
    * */
    template <typename CU, 
            bool = is_empty_cu<CU>::value>
    class to_str_type_t;

    template <typename CU>
    class to_str_type_t<CU, false>
    {
    private:
        static_assert(cu_interface_satisfies_all<CU>::value, "CU consists of invalid types for this interface");
        using type = front_t<CU>;

    public:
        static std::string str() 
        {
            return std::string(type::str()) + "^" + std::to_string(type::degree) +
                to_str_type_t<pop_front_t<CU>>::str();
        }
    };

    template <typename CU>
    class to_str_type_t<CU, true>
    {
    public:
        static std::string str() 
        {
            return "";
        }
    };

    /* 
    * merge_t <compound_unit, compound_unt>
    * is a meta function that has two compound_units as its
    * template arguments and the resulting type is a compound_unit
    * that represents the concatanation of the two arguments. It consists 
    * of a base case (when the second argument is empty) and of a recursive 
    * case (otherwise), which calls the metafunction recursevly. It calls 
    * push_back_t for every element of the second argument, until there are 
    * elements left. The last template block is a wraper for the function.
    * */
    template <typename LCU,
            typename RCU,
            bool = is_empty_cu<RCU>::value>
    class merge;

    template <typename LCU,
              typename RCU>
    class merge<LCU, RCU, false>
    {
    private:
        static_assert(cu_interface_satisfies_all<LCU>::value, "LCU consists of invalid types for this interface");
        static_assert(cu_interface_satisfies_all<RCU>::value, "RCU consists of invalid types for this interface");    
    
        using local = push_front_t<LCU, front_t<RCU>>;
    public:
        using type = typename merge<local, pop_front_t<RCU>>::type;
    };

    template <typename LCU,
            typename RCU>
    class merge<LCU, RCU, true>
    {
    public:
        using type = LCU;
    };

    template <typename LCU,
            typename RCU>
    using merge_t = typename merge<LCU, RCU>::type;

    /*
    * scalar_degree_add_t<base_unit, int>
    * is a meta function that has a type derived from base unit 
    * and an integer as its template arguments, and it returns 
    * a type derived from the same generator type as the input 
    * unit, which has the sum of the input integer and the 
    * degree of the input unit. The second template is a wraper
    * for this function.
    * */
    template <typename U, int A>
    class scalar_degree_add
    {
    private:
        static_assert(interface_satisfies_all_v<U>, "U does not satisfy the interface");
    public:
        using type = typename U::template inner_type< A + U::degree>;
                    
    };

    template <typename CU, int A>
    using scalar_degree_add_t = typename scalar_degree_add<CU, A>::type;


    /*
    * find_n_mod <compound_unit, new_element>
    * is a meta function that has a compound_unit and an element
    * which is a type that satisfies the compoun_unit interface, 
    * and returns a compound_unit which is modified in one of the 
    * following two ways:
    * 
    * (1) if new_element has the same generator type as another 
    *     type in the compound_unit, then the scalar_degree_add
    *     meta function is called for the element in the 
    *     compound_unit and the degree of the input unit
    * 
    * (2) otherwise, the new element will be appended on the input
    *     compound_unit.
    * 
    * The last template is a wraper for find_n_mod.
    * */
    template <typename CU, 
              typename Element,
              bool Empty = is_empty_cu<CU>::value>
    class find_n_mod;

    template <typename CU,
              typename Element>
    class find_n_mod<CU, Element, false>
    {
    private:
        static_assert(cu_interface_satisfies_all<CU>::value, "CU consists of invalid types for this interface");
        static_assert(interface_satisfies_all_v<Element>, "Element does not satisfy the interface");
    
        using head = front_t<CU>;

    public:
        using type = std::conditional_t< std::is_same_v< typename Element::template inner_type<0>,
                                                         typename head::template inner_type<0>
                                                       >,
                                         push_front_t<pop_front_t<CU>, scalar_degree_add_t<head, Element::degree>>,
                                         push_front_t<typename find_n_mod<pop_front_t<CU>, Element>::type, head>
                                       >;
    };

    template <typename CU,
            typename Element>
    class find_n_mod<CU, Element, true>
    {
    public:
        using type = push_front_t<CU, Element>;
    };

    template <typename CU, typename Element>
    using find_n_mod_t = typename find_n_mod<CU, Element>::type;
    
    
    /*
    * multiply <compound_unit, compound_unit>
    * is a meta function that has two compound_units as
    * template arguments and returns the product of the 
    * two compound_units. It consists of two parts: the 
    * recursice call (which calls the find_n_mod_t meta
    * function for every element of the second argument)
    * and a base case when the second argument is empty.
    * The last template is a wraper for the multiply 
    * function.
    * */

    template <typename LCU,
            typename RCU,
            bool = is_empty_cu<RCU>::value>
    class multiply;

    template <typename LCU,
              typename RCU>
    class multiply<LCU, RCU, false>
    {
    private:
        static_assert(cu_interface_satisfies_all<LCU>::value, "LCU consists of invalid types for this interface");
        static_assert(cu_interface_satisfies_all<RCU>::value, "RCU consists of invalid types for this interface");

        using local = find_n_mod_t<LCU, front_t<RCU>>;
        using another = typename multiply<local, pop_front_t<RCU>>::type;
    public:
        using type = another;
    };

    template <typename LCU,
              typename RCU>
    class multiply<LCU, RCU, true>
    {
    public:
        using type = LCU;
    };

    template <typename LCU, typename RCU>
    using multiply_t = typename multiply<LCU, RCU>::type;


    /*
    * negate_degree_sign
    * is a meta function that has a type derived from the 
    * base_unit class and returns a unit generated from the 
    * same type generator type as the input unit, with the 
    * negated degree of the input unit. The last template 
    * is a wraper for the negate_degree_sign function.
    * */
    template <typename U>
    class negate_degree_sign
    {
    private:
        static_assert(interface_satisfies_all_v<U>, "U consists of invalid types for this interface");
    public:
        using type = typename U::template inner_type< -U::degree>;
                    
    };

    template <typename CU>
    using negate_degree_sign_t = typename negate_degree_sign<CU>::type;

    /*
    * negate_cu_degree <compound_unit>
    * is a meta function that calls the negate_degree_sign_t function 
    * for every element of compound_unit and returns the resulting 
    * compound_unit. It consists of the recursive case, that applies 
    * the negate_degree_sign on every element, and of a base case, when
    * the input compound_unit is empty. The last template is a wraper 
    * for the meta function.
    * */

    template <typename CU,
            bool  = is_empty_cu<CU>::value>
    class negate_cu_degree;

    template <typename CU>
    class negate_cu_degree<CU, false>
    {
    private:
        static_assert(cu_interface_satisfies_all<CU>::value, "CU consists of invalid types for this interface");
        using local = typename negate_cu_degree<pop_front_t<CU>>::type;
    public:
        using type = push_front_t<local, negate_degree_sign_t<front_t<CU>>>;

    };

    template <typename CU>
    class negate_cu_degree<CU, true>
    {
    public:
        using type = CU;
    };

    template <typename CU>
    using negate_cu_degree_t = typename negate_cu_degree<CU>::type;


    /*
    * divide_t <compound_unit, compound_unit>
    * is a meta function which has two compound_units as input
    * and computes the quotient of the two. The function first 
    * calls negate_cu_degree_t for the second template argument
    * and then it calls mupliply for the first and modified second 
    * template argument. The second template is a wraper for the 
    * meat function. 
    * */


    template <typename LCU,
            typename RCU>
    class divide
    {
    private:
        static_assert(cu_interface_satisfies_all<LCU>::value, "LCU consists of invalid types for this interface");
        static_assert(cu_interface_satisfies_all<RCU>::value, "RCU consists of invalid types for this interface");
    public:
        using type = multiply_t<LCU, negate_cu_degree_t<RCU>>;
    };


    template <typename LCU, typename RCU>
    using divide_t = typename divide<LCU, RCU>::type;


    /*
    * remove_zero_cu_degree_t <compound_unit>
    * is a meta function that has a compound_unit
    * as it template argument and returns the modified
    * argument, so that the input argument does not 
    * have units that have zero as their degree.
    * It consists of a recursive case where we check if
    * the unit's degree is equal to zero and, if it is,
    * it is removed from the compound_unit, and a base case
    * when the compound_unit is empty. The last template is a 
    * wraper for the meta function.
    * */

    template <typename CU,
            bool  = is_empty_cu<CU>::value>
    class remove_zero_cu_degree;

    template <typename CU>
    class remove_zero_cu_degree<CU, false> 
    {
    private:
        static_assert(cu_interface_satisfies_all<CU>::value, "CU consists of invalid types for this interface");
        using local = typename remove_zero_cu_degree<pop_front_t<CU>>::type;

    public:
        using type = std::conditional_t< front_t<CU>::degree == 0,
                                local,
                                push_front_t<local, front_t<CU>>
                                >;
    };

    template <typename CU>
    class remove_zero_cu_degree<CU, true>
    {
    public:
        using type = CU;
    };

    template <typename CU>
    using remove_zero_cu_degree_t = typename remove_zero_cu_degree<CU>::type;


    /*
    * tail_or_head_t
    * is a meta helper function (for the meta function unfold_t)
    * which has a compound_unit and a bool as its template
    * arguments. The first argument must have a compound_unit
    * as its first element.
    * The second argument is deduced from the first
    * element of the first argument and it holds the value true 
    * or false, based on the fact if the first argument of the 
    * passed compound_unit is empty or not.
    * The meta function works like this:
    *   (1) If the first element (head) is an empty compound_unit, 
    *       the first element will be discarded.
    *   (2) Otherwise, the first element of the head will be appended
    *       to the compound_unit, and the meta function will be recursevly
    *       called for the newly formed compound_unit.
    * */
    template <typename CU, bool = is_empty_cu<front_t<CU>>::value>
    class tail_or_head;

    template <typename CU>
    class tail_or_head<CU, false>
    {
    private:

        using head = front_t<CU>;
        using local_head = front_t<head>;
        using local_tail = pop_front_t<head>;
        using local_CU = push_front_t<pop_front_t<CU>, local_tail>;
    public:
        using type = push_front_t<local_CU, local_head>;
    };

    template <typename CU>
    class tail_or_head<CU, true>
    {
    public:
        using type = pop_front_t<CU>;
    };

    /*
    * This is just a wraper for the meta function that produces the 
    * resulting type of the meta function tail_or_head.
    * */
    template <typename CU>
    using tail_or_head_t = typename tail_or_head<CU>::type;


    /*
    * try_front_t 
    * is a helper meta function (for the meta function unfold)
    * which tries to perform the operation front_t on
    * the passsed argument. 
    * If the passed compound_unit is not empty, then the function
    * performs the operation front_t. Otherwise it returns an empty
    * compound_unit.
    * */

    template <typename CU,
            bool = is_empty_cu<CU>::value>
    class try_front;

    template <typename CU>
    class try_front<CU, false>
    {
    public:
        using type = front_t<CU>;
    };

    template <typename CU>
    class try_front<CU, true>
    {
    public:
        using type = compound_unit<>;
    };

    /*
    * This is a wraper for the try_front meta function which 
    * produces the resulting type of the meta function try_front.
    * */
    template <typename CU>
    using try_front_t = typename try_front<CU>::type;



    /*
    * unfold_t 
    * is a meta function which, for a passed compound_unit,
    * that consists of a list of elemets, which may be other 
    * compound_units and base_units, returns a compound_unit
    * whose elements are only base_units, derived from the 
    * argument. Basicaly it collects all base_units from the 
    * argument (nested or not) int a compound_unit, which 
    * is returned.
    * The meta function works like this:
    *   (1) If the passed argument is not empty and if its
    *       first element is of type base_unit, it will return
    *       a compound_unit whose first element is the first 
    *       element of the function argument and the rest is
    *       derived from the recursive call on the other elements
    *       of the argument tail.
    *   (2) If the passed argument is not empty and if its
    *       first element is not of type base_unit, it will
    *       return the result of the recursivly called function,
    *       whose argument is the result of the meta function 
    *       tail_or_head_t.
    *   (3) Otherwise, the function will return the argument.
    * 
    * */


    template <typename CU,
            bool = is_empty_cu<CU>::value,
            bool = has_degree_t<try_front_t<CU>>::value>
    class unfold;

    template <typename CU>
    class unfold<CU, false, true>
    {
    private:
        using head = front_t<CU>;
        using tail = pop_front_t<CU>;

    public:
    using type = push_front_t<typename unfold<tail>::type, head>;

    };

    template <typename CU>
    class unfold<CU, false, false>
    {
    private:
        using head = front_t<CU>;
        using tail = pop_front_t<CU>;

    public: 

    using type = typename unfold<tail_or_head_t<CU>>::type;

    };

    template <typename CU>
    class unfold<CU, true, false>
    {
    public:
        using type = CU;
    };


    /* 
    * This is just a wraper for the meta function unfold,
    * which produces the resulitng type of the meta function
    * unfold.
    * */
    template <typename CU>
    using unfold_t = typename unfold<CU>::type;



    /*
    * condense_t 
    * is a meta function which calculates the sum of the degrees
    * of the related base_unit types in the argument and then puts 
    * those base_units into a resulting compound_unit.
    * It calls the meta function multiply_t for an empty
    * compound_unit and the passed argument and its resulting type
    * is the resulting type of the condense meta function.
    * */

    template <typename CU>
    class condense 
    {
    public:
        using type = multiply_t<compound_unit<>, CU>;
    };

    /* 
    * This is just a wraper for the meta function condense,
    * which produces the resulitng type of the meta function
    * condense.
    * */
    template <typename CU>
    using condense_t = typename condense<CU>::type;

}

#endif