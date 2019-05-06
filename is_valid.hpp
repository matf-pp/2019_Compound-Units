#ifndef IS_VALID_HPP
#define IS_VALID_HPP
#include <type_traits>
#include <utility>

/* 
 * This file contains generic functions and types that are  
 * needed for the definition of type traits. 
 */

namespace valid {

    /*
     * is_valid is a generic functoin, whose argument is a lambda, and if the 
     * lambda can evaluate a result, the function will return std::true_type,
     * else it will return :: std::false_type. 
     * is_valid is implemented using the is_valid_impl interface. The return 
     * type of is_valid is actually a lambda closure type, which calls a lambda
     * with forwarded arguments from the is_valid interface, whose return type
     * is again a lambda closure type. That nested lambda actually determines 
     * if the lambda, which has the expression that needs to be evaluated, 
     * can be evaluated. Here type_t is used as a helper template so that we can 
     * represent a type as a value; type is a wraper for representing a type 
     * as a value; value_t is a helper for unwraping a wraped type in a 
     * unevaluated context.
     * */
    template <typename F, typename... Args,
            typename = decltype(std::declval<F>()(std::declval<Args&&>()...))>
    std::true_type is_valid_impl(void*);

    template <typename F, typename... Args>
    std::false_type is_valid_impl(...);

    inline constexpr 
    auto is_valid = [] (auto f) {
                        return  [] (auto&& ... args) {
                            return decltype(is_valid_impl<decltype(f),
                                                        decltype(args)&&...
                                                    >(nullptr)){};
                        };
                    };

    template <typename T>
    struct type_t {
        using Type = T;
    };

    template <typename T>
    constexpr auto type = type_t<T>{};

    template <typename T>
    T value_t(type_t<T>);


    /*
     * satisfies_all_v is a generic class (type) which represents the
     * conjunction of values of Ps templates with the T template argument.
     * It is used in our case for type_trait conjunction.
     * */

    template <class T, template <class> class... Ps>
    class satisfies_all_v : public std::conjunction<Ps<T>...> {};
}

#endif