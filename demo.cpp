#include "compound_unit.hpp"
#include <iostream>

/*
 * This program represents a demo for the "unit" API.
 * */

int main(int argc, char** argv)
{

    /*
    * This compound_unit represents the unit for acceleration.
    * */
    using acceleration = unit::compound_unit<unit::meter<1>, unit::second<-2>>;
    using cout_format = unit::to_str_type_t<acceleration>;
    std::cout << "Acceleration:\n" 
              << cout_format::str() 
              << std::endl << std::endl;

    /*
    * Here we concatanate two compound_units:
    * acceleration and an unnamed cu.
    * */
    using merged = unit::merge_t<acceleration, 
                                 unit::compound_unit<unit::second<3>, 
                                                     unit::second<2>>>;
    using merged_cout_format = unit::to_str_type_t<merged>;
    std::cout << "Merged two compound_units:\n" 
              << merged_cout_format::str()
              << std::endl << std::endl;

    /*
    * Since the resulting compound_unit has multiple entries
    * for second, we are going to condense it, so that we get 
    * a compound_unit with only one entry for each unit with
    * the resulting degree.
    * */
    using condensed = unit::condense_t<merged>;
    using condensed_cout_format = unit::to_str_type_t<condensed>;
    std::cout << "Condensing the two merged ones:\n" 
              << condensed_cout_format::str()
              << std::endl << std::endl;

    using some_unit = unit::compound_unit<unit::second<-1>, unit::meter<4>>;

    /*
    * Here we multiply two compound_units:
    * acceleration and some_unit.
    * */
    using multiplied = unit::multiply_t<acceleration, 
                                        some_unit>;
    using multiplied_cout_format = unit::to_str_type_t<multiplied>;
    std::cout << "Multiplying two compound_units:\n" 
              << multiplied_cout_format::str() 
              << std::endl << std::endl;

    /*
    * Here we divide the result of the multiplication
    * with some_unit, so that we should get the
    * acceleration unit again.
    * */
    using divided = unit::divide_t<multiplied, 
                                   some_unit>;
    using divided_cout_format = unit::to_str_type_t<divided>;
    std::cout << "Dividing two compound_units:\n" 
              << divided_cout_format::str() 
              << std::endl << std::endl;

    /*
    * Here we divide again, but we spot a problemm:
    * we have an unit that has a degree equal to zero.
    * */
    using divided_again = unit::divide_t<divided,
                                         unit::compound_unit<unit::second<-2>>>;
    using divided_again_cout_format = unit::to_str_type_t<divided_again>;
    std::cout << "Dividing again two compound_units:\n" 
              << divided_again_cout_format::str() 
              << std::endl << std::endl;

    /*
    * Here we are removing all units whose degree is equal 
    * to zero.
    * */
    using removed_zero = unit::remove_zero_cu_degree_t<divided_again>;
    using removed_zero_cout_format = unit::to_str_type_t<removed_zero>;
    std::cout << "Removing degrees that are equal to zero:\n" 
              << removed_zero_cout_format::str() 
              << std::endl << std::endl;

    /*
    * Defining a nested compound_unit
    * (a compound_unit whose elements 
    * can also be other compound_units).
    * */
    using nested_cu = unit::compound_unit< unit::meter<2>, 
                                           unit::compound_unit<>, 
                                           unit::compound_unit<unit::meter<6>, 
                                                               unit::compound_unit<unit::second<-4>,
                                                                                   unit::meter<-8>>>>;
    /*
    * Here we are unfolding a nested compound_unit.
    * */
    using unfolded = unit::unfold_t<nested_cu>;
    using unfolded_cout_format = unit::to_str_type_t<unfolded>;
    std::cout << "Unfolding a nested compound_unit:\n" 
              << unfolded_cout_format::str() 
              << std::endl << std::endl;

    /*
    * Since the resulting compound_unit is not condensed
    * and potentially has degrees that are equal to zero,
    * we are here formating the resulting compound_unit.
    * */
    using formated_h = unit::condense_t<unfolded>;
    using formated_f = unit::remove_zero_cu_degree_t<formated_h>;
    using formated_f_cout_format = unit::to_str_type_t<formated_f>;
    std::cout << "Formating the unfolded compound_unit:\n" 
              << formated_f_cout_format::str()
              << std::endl << std::endl;


    return 0;
}
