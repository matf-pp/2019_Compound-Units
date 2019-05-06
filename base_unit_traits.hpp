#ifndef UNIT_TRAITS_HPP
#define UNIT_TRAITS_HPP
#include "is_valid.hpp"

/* 
 * This file contains type traits that enforce 
 * the correct use of the "unit" API. 
 */

namespace unit {

    /* 
     * has_degree_t type trait
     * 
     * This type trait checks if a forwarded type has the static field 
     * degree.
     * The implementation consists of a lambda which is the return type 
     * is_valid, and the definition of the type trait itself, where we
     * call lambda with the template argument.
     * */
    constexpr auto has_degree
    = valid::is_valid([] (auto&& x) -> decltype((void)&x.degree) {});
    
    template <typename T>
    using has_degree_t = decltype(has_degree(std::declval<T>()));

    /* 
     * has_str_method_t type trait
     * 
     * This type trait checks if a forwarded type has the static function 
     * str().
     * The implementation consists of a lambda which is the return type 
     * is_valid, and the definition of the type trait itself, where we
     * call lambda with the template argument.
     * */

    constexpr auto has_str_method
    = valid::is_valid([] (auto&& x) -> decltype(x.str()) {});
    
    template <typename T>
    using has_str_method_t = decltype(has_str_method(std::declval<T>()));
    
    /* 
     * has_inner_type_template_t type trait
     * 
     * This type trait checks if a forwarded type has the template type 
     * inner_type.
     * The implementation consists of a lambda which is the return type 
     * is_valid, and the definition of the type trait itself, where we
     * call lambda with the template argument.
     * */

    constexpr auto has_inner_type_template
    = valid::is_valid([] (auto&& x) -> typename std::decay_t<decltype(x)>::template inner_type<0> {});

    template <typename T>
    using has_inner_type_template_t 
    = decltype(has_inner_type_template(std::declval<T>()));

    /* 
     * interface_satisfies_all_v type trait
     * 
     * This type trait checks if a all forwarded type traits evaluate true for
     * that forwarded type.
     * The implementation consists of an alias template that specializes what 
     * type traits are conjuncted in the call of valid::satisfies_all_v and 
     * a wraper for that alias template.
     * */

    template <typename T>
    using interface_satisfies_all = valid::satisfies_all_v< T, has_degree_t, 
                                                               has_str_method_t,
                                                               has_inner_type_template_t>;

    template <typename T>
    constexpr bool interface_satisfies_all_v = interface_satisfies_all<T>::value;

}
#endif