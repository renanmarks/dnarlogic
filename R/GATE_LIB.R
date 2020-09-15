#library("DNAr")

# Helper function to concat strings
jn <- function(...) { paste(..., sep = '') }

#' Create the singals' names of DNA species.
#'
#' @export
#' @param name The signal name.
#' @return List of signals' names.
make_signal_species <- function(name) {
  # Define the dual-rail
  species <- list(
    value0 = jn(name, '_0'),
    value1 = jn(name, '_1'),
    switch = jn(name, '_S')
  )

  return(species)
}

#' Create signal to use in the circuit.
#'
#' @export
#' @param name The signal name.
#' @return A signal with its name, species, CRN reactions and ki constants.
make_signal <- function(name) {
  species <- make_signal_species(name)

  reactions <- list(
    jn(species$value0, ' + ', species$value1, ' -> ', species$switch),
    jn(species$switch, ' + ', species$value0, ' -> 3', species$value0),
    jn(species$switch, ' + ', species$value1, ' -> 3', species$value1)
  )

  ki <- c(1E+4, 1E+4, 1E+4)

  signal <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ki        = ki
  )

  return(signal)
}

#' Create a 2-inputs and 1-output generic element.
#'
#' @export
#' @param name The logic gate name
#' @param typename The type name of element (Latch, Flipflop, AND, OR, XOR, ...)
#' @param in1str The string identifier of input1 signal.
#' @param input1value The initial value of input1.
#' @param in2str The string identifier of input2 signal.
#' @param input2value The initial value of input2.
#' @param outstr The string identifier of output signal.
#' @return A generic logic gate with its name, species, CRN reactions, ki and ci constants.
make_generic2to1_element <- function (name, typename, in1str, input1value, in2str, input2value, outstr) {
  input1 <- make_signal(jn(name, '_', typename, '_', in1str))
  input2 <- make_signal(jn(name, '_', typename, '_', in2str))
  output <- make_signal(jn(name, '_', typename, '_', outstr))

  species <- list(input1 = input1$species, input2 = input2$species, output = output$species)

  ci <- list((1 - input1value) * 1E-7, # _in1_0
          (    input1value) * 1E-7, # _in1_1
          0,    # _in1_S

          (1 - input2value) * 1E-7, # _in2_0
          (    input2value) * 1E-7, # _in2_1
          0,    # _in2_S

          1E-7, # _out_0
          0,    # _out_1
          0     # _out_S
  )

  reactions <- list(input1$reactions, input2$reactions, output$reactions)
  ki        <- list(input1$ki, input2$ki, output$ki)

  gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ki        = ki,
    ci        = ci
  )

  return(gate)
}

#' Create a 2-inputs and 1-output generic logic gate.
#'
#' @export
#' @param name The logic gate name
#' @param typename The type name of logic gate (AND, OR, XOR, ...)
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return A generic logic gate with its name, species, CRN reactions, ki and ci constants.
make_generic2to1_gate <- function (name, typename, input1value, input2value) {
  return(make_generic2to1_element(name, typename, 'in1', input1value, 'in2', input2value, 'out'))
}

#' Create a 3-inputs and 1-output generic element.
#'
#' @export
#' @param name The logic gate name
#' @param typename The type name of element (Latch, Flipflop, AND, OR, XOR, ...)
#' @param in1str The string identifier of input1 signal.
#' @param input1value The initial value of input1.
#' @param in2str The string identifier of input2 signal.
#' @param input2value The initial value of input2.
#' @param in3str The string identifier of input3 signal.
#' @param input3value The initial value of input3.
#' @param outstr The string identifier of output signal.
#' @return A generic logic gate with its name, species, CRN reactions, ki and ci constants.
make_generic3to1_element <- function (name, typename, in1str, input1value, in2str, input2value, in3str, input3value, outstr) {
  input1 <- make_signal( jn(name, '_', typename, '_', in1str) )
  input2 <- make_signal( jn(name, '_', typename, '_', in2str) )
  input3 <- make_signal( jn(name, '_', typename, '_', in3str) )
  output <- make_signal( jn(name, '_', typename, '_', outstr) )

  species <- list(input1 = input1$species, input2 = input2$species, input3 = input3$species, output = output$species)

  ci <- list((1 - input1value) * 1E-7, # _in1_0
          (    input1value) * 1E-7, # _in1_1
          0,    # _in1_S

          (1 - input2value) * 1E-7, # _in2_0
          (    input2value) * 1E-7, # _in2_1
          0,    # _in2_S

          (1 - input3value) * 1E-7, # _in3_0
          (    input3value) * 1E-7, # _in3_1
          0,    # _in3_S

          1E-7, # _out_0
          0,    # _out_1
          0     # _out_S
  )

  reactions <- list(input1$reactions, input2$reactions, input3$reactions, output$reactions)
  ki        <- list(input1$ki, input2$ki, input3$ki, output$ki)

  gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ki        = ki,
    ci        = ci
  )

  return(gate)
}

#' Create a 3-inputs and 1-output generic logic gate.
#'
#' @export
#' @param name The logic gate name
#' @param typename The type name of logic gate (AND, OR, XOR, ...)
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @param input3value The initial value of input3.
#' @return A generic logic gate with its name, species, CRN reactions, ki and ci constants.
make_generic3to1_gate <- function (name, typename, input1value, input2value, input3value) {
  return(make_generic3to1_element(name, typename, 'in1', input1value, 'in2', input2value, 'in3', input3value, 'out'))
}

#' Create an MAJORITY gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @param input3value The initial value of input3.
#' @return An AND gate with its name, species, specific CRN reactions, ki and ci constants.
make_majority_gate <- function(name, input1value, input2value, input3value) {
  gate <- make_generic3to1_gate(name, 'MAJ', input1value, input2value, input3value)

  gate$species   <- rlist::list.append(gate$species, list(outTo1 = jn(name, '_MAJ_out_to_1'), outTo0 = jn(name, '_MAJ_out_to_0')))
  gate$ci        <- rlist::list.append(gate$ci, list(0, 0))
  gate$reactions <- rlist::list.append(gate$reactions, list(

                      # 'A1 + B1 -> A1 + B1 + Z_1',
                      jn(gate$species$input1$value1, ' + ', gate$species$input2$value1, ' -> ',
                         gate$species$input1$value1, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo1 ),
                      # 'A1 + C1 -> A1 + C1 + Z_1',
                      jn(gate$species$input1$value1, ' + ', gate$species$input3$value1, ' -> ',
                         gate$species$input1$value1, ' + ', gate$species$input3$value1, ' + ', gate$species$outTo1 ),
                      # 'B1 + C1 -> B1 + C1 + Z_1',
                      jn(gate$species$input2$value1, ' + ', gate$species$input3$value1, ' -> ',
                         gate$species$input2$value1, ' + ', gate$species$input3$value1, ' + ', gate$species$outTo1 ),

                      # 'A0 + B0 -> A0 + B0 + Z_0',
                      jn(gate$species$input1$value0, ' + ', gate$species$input2$value0, ' -> ',
                         gate$species$input1$value0, ' + ', gate$species$input2$value0, ' + ', gate$species$outTo0 ),

                      # 'B0 + C0 -> B0 + C0 + Z_0',
                      jn(gate$species$input2$value0, ' + ', gate$species$input3$value0, ' -> ',
                         gate$species$input2$value0, ' + ', gate$species$input3$value0, ' + ', gate$species$outTo0 ),

                      # 'A0 + C0 -> A0 + C0 + Z_0',
                      jn(gate$species$input1$value0, ' + ', gate$species$input3$value0, ' -> ',
                         gate$species$input1$value0, ' + ', gate$species$input3$value0, ' + ', gate$species$outTo0 ),

                      # 'Z_1 + Z0 -> Z1'
                      jn(gate$species$outTo1, ' + ', gate$species$output$value0, ' -> ', gate$species$output$value1),
                      # 'Z_0 + Z1 -> Z0'
                      jn(gate$species$outTo0, ' + ', gate$species$output$value1, ' -> ', gate$species$output$value0),

                      # '2Z_0 -> 0',
                      # '2Z_1 -> 0',
                      jn('2', gate$species$outTo0 , ' -> 0'),
                      jn('2', gate$species$outTo1 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4))

  return(gate)
}

#' Create an AND gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return An AND gate with its name, species, specific CRN reactions, ki and ci constants.
make_and_gate <- function(name, input1value, input2value) {
  gate <- make_generic2to1_gate(name, 'AND', input1value, input2value)

  gate$species   <- rlist::list.append(gate$species, list (outTo1 = jn(name, '_AND_out_to_1')))
  gate$ci        <- rlist::list.append(gate$ci, list(0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'X0 + Z1 -> X0 + Z0',
    jn(gate$species$input1$value0, ' + ', gate$species$output$value1, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$output$value0 ),

    # 'Y0 + Z1 -> Y0 + Z0',
    jn(gate$species$input2$value0, ' + ', gate$species$output$value1, ' -> ',
       gate$species$input2$value0, ' + ', gate$species$output$value0 ),

    # 'X1 + Y1 -> X1 + Y1 + Z_1',
    jn(gate$species$input1$value1, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo1 ),

    #'Z_1 + Z0 -> Z1'
    jn(gate$species$outTo1, ' + ', gate$species$output$value0, ' -> ', gate$species$output$value1),

    # '2Z_1 -> 0',
    jn('2', gate$species$outTo1 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
          1E+4,
          1E+4,
          1E+4,
          1E+4,
          1E+4))

  return(gate)
}

#' Create an NAND gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return An NAND gate with its name, species, specific CRN reactions, ki and ci constants.
make_nand_gate <- function(name, input1value, input2value) {
  gate <- make_generic2to1_gate(name, 'NAND', input1value, input2value)

  gate$species   <- rlist::list.append(gate$species, list(outTo0 = jn(name, '_NAND_out_to_0')))
  gate$ci        <- rlist::list.append(gate$ci, list(0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'X0 + Z0 -> X0 + Z1',
    jn(gate$species$input1$value0, ' + ', gate$species$output$value0, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$output$value1 ),

    # 'Y0 + Z0 -> Y0 + Z1',
    jn(gate$species$input2$value0, ' + ', gate$species$output$value0, ' -> ',
       gate$species$input2$value0, ' + ', gate$species$output$value1 ),

    # 'X1 + Y1 -> X1 + Y1 + Z_0',
    jn(gate$species$input1$value1, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo0 ),

    #'Z_0 + Z1 -> Z0'
    jn(gate$species$outTo0, ' + ', gate$species$output$value1, ' -> ', gate$species$output$value0),

    # '2Z_0 -> 0',
    jn('2', gate$species$outTo0 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4))

  return(gate)
}

#' Create an OR gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return An OR gate with its name, species, specific CRN reactions, ki and ci constants.
make_or_gate <- function(name, input1value, input2value) {
  gate <- make_generic2to1_gate(name, 'OR', input1value, input2value)

  gate$species   <- rlist::list.append(gate$species, list(outTo0 = jn(name, '_OR_out_to_0')))
  gate$ci        <- rlist::list.append(gate$ci, list(0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'X1 + Z0 -> X1 + Z1',
    jn(gate$species$input1$value1, ' + ', gate$species$output$value0, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$output$value1 ),

    # 'Y1 + Z0 -> Y1 + Z1',
    jn(gate$species$input2$value1, ' + ', gate$species$output$value0, ' -> ',
       gate$species$input2$value1, ' + ', gate$species$output$value1 ),

    # 'X0 + Y0 -> X0 + Y0 + Z_0',
    jn(gate$species$input1$value0, ' + ', gate$species$input2$value0, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$input2$value0, ' + ', gate$species$outTo0 ),

    #'Z_0 + Z1 -> Z0'
    jn(gate$species$outTo0, ' + ', gate$species$output$value1, ' -> ', gate$species$output$value0),

    # '2Z_0 -> 0',
    jn('2', gate$species$outTo0 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4))

  return(gate)
}

#' Create an NOR gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return An NOR gate with its name, species, specific CRN reactions, ki and ci constants.
make_nor_gate <- function(name, input1value, input2value) {
  gate <- make_generic2to1_gate(name, 'NOR', input1value, input2value)

  gate$species   <- rlist::list.append(gate$species, list(outTo1 = jn(name, '_NOR_out_to_1')))
  gate$ci        <- rlist::list.append(gate$ci, list(0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'X1 + Z1 -> X1 + Z0',
    jn(gate$species$input1$value1, ' + ', gate$species$output$value1, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$output$value0 ),

    # 'Y1 + Z1 -> Y1 + Z0',
    jn(gate$species$input2$value1, ' + ', gate$species$output$value1, ' -> ',
       gate$species$input2$value1, ' + ', gate$species$output$value0 ),

    # 'X0 + Y0 -> X0 + Y0 + Z_1',
    jn(gate$species$input1$value0, ' + ', gate$species$input2$value0, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$input2$value0, ' + ', gate$species$outTo1 ),

    #'Z_1 + Z0 -> Z1'
    jn(gate$species$outTo1, ' + ', gate$species$output$value0, ' -> ', gate$species$output$value1),

    # '2Z_1 -> 0',
    jn('2', gate$species$outTo1 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,
               1E+4,
               1E+4,
               1E+4))

  return(gate)
}

#' Create an XOR gate with initial input values.
#'
#' @export
#' @param name The logic gate name
#' @param input1value The initial value of input1.
#' @param input2value The initial value of input2.
#' @return An XOR gate with its name, species, specific CRN reactions, ki and ci constants.
make_xor_gate <- function(name, input1value, input2value) {
  gate <- make_generic2to1_gate(name, 'XOR', input1value, input2value)

  gate$species   <- rlist::list.append(gate$species, list(outTo1 = jn(name, '_XOR_out_to_1')))
  gate$species   <- rlist::list.append(gate$species, list(outTo0 = jn(name, '_XOR_out_to_0')))
  gate$ci        <- rlist::list.append(gate$ci, list(0, 0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'X0 + Y1 -> X0 + Y1 + Z_1',
    jn(gate$species$input1$value0, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo1 ),

    # 'X1 + Y0 -> X1 + Y0 + Z_1',
    jn(gate$species$input1$value1, ' + ', gate$species$input2$value0, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$input2$value0, ' + ', gate$species$outTo1 ),

    #'Z_1 + Z0 -> Z1'
    jn(gate$species$outTo1, ' + ', gate$species$output$value0, ' -> ', gate$species$output$value1),

    # '2Z_1 -> 0',
    jn('2', gate$species$outTo1 , ' -> 0'),



    # 'X0 + Y0 -> X0 + Y0 + Z_0',
    jn(gate$species$input1$value0, ' + ', gate$species$input2$value0, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$input2$value0, ' + ', gate$species$outTo0 ),

    # 'X1 + Y1 -> X1 + Y1 + Z_0',
    jn(gate$species$input1$value1, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo0 ),

    #'Z_0 + Z1 -> Z0'
    jn(gate$species$outTo0, ' + ', gate$species$output$value1, ' -> ', gate$species$output$value0),

    # '2Z_0 -> 0',
    jn('2', gate$species$outTo0, ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,
               1E+4,
               1E+4,

               1E+4,
               1E+4,
               1E+4,
               1E+4))

  return(gate)
}

#' Create an Latch-D with initial input values.
#'
#' @export
#' @param name The Latch-D name
#' @param dValue The initial value of Data signal.
#' @param eValue The initial value of Enable signal
#' @return An Latch-D gate with its name, species, specific CRN reactions, ki and ci constants.
make_latchd <- function(name, dValue, eValue) {
  gate <- make_generic2to1_element(name, 'LD', 'd', dValue, 'en', eValue, 'q')

  gate$species   <- rlist::list.append(gate$species, list(outTo1 = jn(name, '_LD_q_to_1')))
  gate$species   <- rlist::list.append(gate$species, list(outTo0 = jn(name, '_LD_q_to_0')))
  gate$ci        <- rlist::list.append(gate$ci, list(0, 0))
  gate$reactions <- rlist::list.append(gate$reactions, list(
    # 'D0 + E1 -> D0 + E1 + Q_0',
    jn(gate$species$input1$value0, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value0, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo0 ),

    # 'D1 + E1 -> D1 + E1 + Q_1',
    jn(gate$species$input1$value1, ' + ', gate$species$input2$value1, ' -> ',
       gate$species$input1$value1, ' + ', gate$species$input2$value1, ' + ', gate$species$outTo1 ),

    # 'Q_0 + Q1 -> Q0',
    jn(gate$species$outTo0, ' + ', gate$species$output$value1, ' -> ', gate$species$output$value0),

    # 'Q_1 + Q0 -> Q1'
    jn(gate$species$outTo1, ' + ', gate$species$output$value0, ' -> ', gate$species$output$value1),

    # '2Q_0 -> 0',
    jn('2', gate$species$outTo0 , ' -> 0'),

    # '2Q_1 -> 0',
    jn('2', gate$species$outTo1 , ' -> 0')
  ))

  gate$ki <- rlist::list.append(gate$ki, list(
               1E+4,
               1E+4,

               1E+4,
               1E+4,

               1E+4,
               1E+4))

  return(gate)
}

#' Create an FlipFlop-D with initial input values.
#'
#' @export
#' @param name The FlipFlop-D name
#' @param dValue The initial value of Data signal.
#' @param eValue The initial value of Enable signal
#' @return An FlipFlop-D gate with its name, species, specific CRN reactions, ki and ci constants.
make_flipflopd <- function(name, dValue, eValue) {
  # M is first Latch-D, Q is the second Latch-D
  ld1 <- make_generic2to1_element(name, 'LD1', 'd', dValue, 'en', eValue, 'm')
  ld2 <- make_generic2to1_element(name, 'LD2', 'd', dValue, 'en', eValue, 'q')

  # First Latch
  ld1$species   <- rlist::list.append(ld1$species, list(outTo1 = jn(name, '_LD_m_to_1')))
  ld1$species   <- rlist::list.append(ld1$species, list(outTo0 = jn(name, '_LD_m_to_0')))

  ld1$ci        <- rlist::list.append(ld1$ci, list(0, 0))
  ld1$reactions <- rlist::list.append(ld1$reactions, list(
    # 'D0 + E0 -> D0 + E0 + M_0',
    jn(ld1$species$input1$value0, ' + ', ld1$species$input2$value0, ' -> ',
       ld1$species$input1$value0, ' + ', ld1$species$input2$value0, ' + ', ld1$species$outTo0 ),

    # 'D1 + E0 -> D1 + E0 + M_1',
    jn(ld1$species$input1$value1, ' + ', ld1$species$input2$value0, ' -> ',
       ld1$species$input1$value1, ' + ', ld1$species$input2$value0, ' + ', ld1$species$outTo1 ),

    # 'M_0 + M1 -> M0',
    jn(ld1$species$outTo0, ' + ', ld1$species$output$value1, ' -> ', ld1$species$output$value0),

    # 'M_1 + M0 -> M1'
    jn(ld1$species$outTo1, ' + ', ld1$species$output$value0, ' -> ', ld1$species$output$value1),

    # '2M_0 -> 0',
    jn('2', ld1$species$outTo0 , ' -> 0'),

    # '2M_1 -> 0',
    jn('2', ld1$species$outTo1 , ' -> 0')
  ))

  ld1$ki <- rlist::list.append(ld1$ki, list(
    1E+4,
    1E+4,

    1E+4,
    1E+4,

    1E+4,
    1E+4))


  ld2$species   <- rlist::list.append(ld2$species, list(outTo1 = jn(name, '_LD_q_to_1')))
  ld2$species   <- rlist::list.append(ld2$species, list(outTo0 = jn(name, '_LD_q_to_0')))

  ld2$ci        <- rlist::list.append(ld2$ci, list(0, 0))
  ld2$reactions <- rlist::list.append(ld2$reactions, list(
    # 'M0 + E0 -> M0 + E1 + Q_0',
    jn(ld1$species$output$value0, ' + ', ld2$species$input2$value1, ' -> ',
       ld1$species$output$value0, ' + ', ld2$species$input2$value1, ' + ', ld2$species$outTo0 ),

    # 'M1 + E0 -> M1 + E1 + Q_1',
    jn(ld1$species$output$value1, ' + ', ld2$species$input2$value1, ' -> ',
       ld1$species$output$value1, ' + ', ld2$species$input2$value1, ' + ', ld2$species$outTo1 ),

    # 'Q_0 + Q1 -> Q0',
    jn(ld2$species$outTo0, ' + ', ld2$species$output$value1, ' -> ', ld2$species$output$value0),

    # 'Q_1 + Q0 -> Q1'
    jn(ld2$species$outTo1, ' + ', ld2$species$output$value0, ' -> ', ld2$species$output$value1),

    # '2Q_0 -> 0',
    jn('2', ld2$species$outTo0 , ' -> 0'),

    # '2Q_1 -> 0',
    jn('2', ld2$species$outTo1 , ' -> 0')
  ))

  ld2$ki <- rlist::list.append(ld2$ki, list(
    1E+4,
    1E+4,

    1E+4,
    1E+4,

    1E+4,
    1E+4))

  gate <- list(
    name      = name,
    species   = list(ld1$species, ld2$species),
    reactions = list(ld1$reactions, ld2$reactions),
    ki        = list(ld1$ki, ld2$ki),
    ci        = list(ld1$ci, ld2$ci)
  )

  return(gate)
}

#' Creates a "link gate", i.e., an object to hold the necessary metadata of linked species.
#'
#' @param sourceSignal The source species to be linked.
#' @param destinySignal The destiny species that will be linked to source species.
#' @param negated If the link have a NOT gate between the source and destiny
#'
#' @return An object holding the necessary metadata of linked species.
#' @export
make_link_gate <- function(sourceSignal, destinySignal, negated = FALSE) {

  valueZero <- destinySignal$value0
  valueOne <- destinySignal$value1

  if (negated)
  {
    valueZero <- destinySignal$value1
    valueOne <- destinySignal$value0
  }

  gate <- list(
    linkedSignals = list(sourceSignal, destinySignal),
    inverted  = negated,
    species   = c(),
    reactions = c( jn(sourceSignal$value1, ' + ', valueZero, ' -> ',
                      sourceSignal$value1, ' + ', valueOne),

                   jn(sourceSignal$value0, ' + ', valueOne, ' -> ',
                      sourceSignal$value0, ' + ', valueZero)
                ),
    ki        = c(1E+4, 1E+4),
    ci        = c()
  )

  return(gate)
}

#' Create an empty circuit.
#'
#' @export
#' @param timing A vector consisting of initial time, final time and timesteps to do the circuit simulation.
#' @return An empty circuit.
make_circuit <- function(timing) {
  circuit <- list(
    gates     = list(),
    species   = c(),
    ci        = c(),
    reactions = c(),
    ki        = c(),
    t         = timing
    # qmax      = gate_qmax,
    # cmax      = gate_cmax,
    # alpha     = gate_alpha,
    # beta      = gate_beta
  )

  return(circuit)
}


#' Compiles the circuit by combining all the species, reactions, initial concentrations and reaction constants.
#' This enables the circuit to be simulated in DNAr.
#'
#' @param circuit Circuit to be compiled.
#'
#' @return Compiled circuit.
#' @export
compile_circuit <- function(circuit) {

  circuit$species   = c()
  circuit$ci        = c()
  circuit$reactions = c()
  circuit$ki        = c()

  for (gate in circuit$gates)
  {
    circuit$species   = append(circuit$species, unlist(gate$species, use.names = FALSE))
    circuit$ci        = append(circuit$ci, gate$ci)
    circuit$reactions = append(circuit$reactions, gate$reactions)
    circuit$ki        = append(circuit$ki, gate$ki)

    not_duplicated_species = !duplicated(circuit$species)
    circuit$species = circuit$species[not_duplicated_species]
    circuit$ci = circuit$ci[not_duplicated_species]
  }

  return(circuit)
}

#' Inserts a gate into the circuit schema without pre-compilation of the circuit.
#'
#' @param circuit Circuit which will receive the gate.
#' @param gate Gate to be inserted into the circuit.
#'
#' @return The circuit with the inserted gate.
#' @export
circuit_insert_gate <- function(circuit, gate) {
  circuit$gates = rlist::list.append(circuit$gates, gate)

  return(circuit)
}

#' Inserts a gate into the circuit schema and pre-compiles the circuit.
#'
#' @export
#' @param circuit Circuit which will receive the gate.
#' @param gate Gate to be inserted into the circuit.
#' @return The circuit with the inserted gate.
circuit_add_gate <- function(circuit, gate) {
  circuit <- circuit_insert_gate(circuit, gate)
  circuit <- compile_circuit(circuit)

  return(circuit)
}

#' Link two signals from logic gates already added into the circuit.
#'
#' @export
#' @param circuit Circuit which will receive the gate.
#' @param outputSignal The output signal to link.
#' @param inputSignal The input signal to link.
#' @return The new circuit with the both signals linked.
circuit_link_gate_signals <- function(circuit, outputSignal, inputSignal) {
  gate <- make_link_gate(sourceSignal = outputSignal, destinySignal = inputSignal, negated = FALSE)
  circuit <- circuit_add_gate(circuit, gate)

  return(circuit)
}

#' Link two signals from logic gates already added into the circuit.
#' This function uses the result from NOT(outputSingal) to link into
#' the asked inputSignal.
#'
#' @export
#' @param circuit Circuit which will receive the gate.
#' @param outputSignal The output signal to link.
#' @param inputSignal The input signal to link.
#' @return The new circuit with the both signals linked.
circuit_link_gate_signals_not <- function(circuit, outputSignal, inputSignal) {
  gate <- make_link_gate(sourceSignal = outputSignal, destinySignal = inputSignal, negated = TRUE)
  circuit <- circuit_add_gate(circuit, gate)

  return(circuit)
}
