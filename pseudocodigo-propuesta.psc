Algoritmo propuesta_final
	Definir nombre_cliente, direccion, letra,funcionamiento_meses, produccion_mensual_entrada,capacidad_planta,respuesta,acumulado_mes_n Como Caracter
	Definir precio_kw Como Real
	Definir valido Como Logico
	precio_kw <- 0.13
	
	Escribir "Algoritmo para calcular el consumo mensual de energía en dólares"
	
	// VALIDACIÓN DE DATOS PERSONALES
	Repetir
		valido <- Verdadero
		
		Escribir "Ingrese el nombre del cliente:"
		Leer nombre_cliente
		Si Longitud(nombre_cliente) = 0 Entonces //Impide que la entrada esté vacía
			valido <- Falso
		Sino
			Para i <- 1 Hasta Longitud(nombre_cliente) //Contador que recorre los carácteres del nombre_cliente
				letra <- Subcadena(nombre_cliente, i, i)
				Si No ((letra >= "A" Y letra <= "Z") O (letra >= "a" Y letra <= "z") O letra = "á" O letra = "é" O letra = "í" O letra = "ó" O letra = "ú" O letra = "ñ" O letra = "Ñ") Entonces
					valido <- Falso  //Validación de carácteres
				FinSi
			FinPara
		FinSi
		Si no valido Entonces
			Escribir "Error: Nombre mal colocado, ingrese letras"
		FinSi
	Hasta Que valido=Verdadero //Si no se ingresan datos válidos, el bucle se repite
	//Repetimos las validaciones anteriores en el inciso a continuación y en los que implican solicitar datos al usuario
	
	//DIRECCIÓN
	Repetir
		valido=Verdadero
		Escribir "Ingrese la ubicación del cliente:"
		Leer direccion
		Si Longitud(direccion) = 0 Entonces
			valido <- Falso
		Sino
			Para i <- 1 Hasta Longitud(direccion)
				character <- SubCadena(direccion, i, i)
				Si No ((character >= "A" Y character <= "Z") O (character >= "a" Y character <= "z") O (character >= "0" Y character <= "9") O character = " ")
					Entonces
                    valido <- Falso
                    Escribir "Carácter inválido encontrado: ", character
                FinSi
            FinPara
        FinSi
		Si No valido Entonces
			Escribir "Error: dirección inválidos. Solo letras y números, sin símbolos."
		FinSi
	Hasta Que valido = Verdadero
	//CAPACIDAD DE LA PLANTA
	Repetir
		valido=Verdadero
		contadorPuntos=0
		Escribir "Ingrese la capacidad de producción la planta (en kw): "
		Leer capacidad_planta
		Si Longitud(capacidad_planta) = 0 Entonces
			valido <- Falso
		Sino
			Para j <- 1 Hasta Longitud(capacidad_planta)
				character <- SubCadena(capacidad_planta, j, j)
				
				Si character = "." Entonces
					contadorPuntos <- contadorPuntos + 1
					Si contadorPuntos > 1 Entonces
						valido <- Falso
					FinSi
				Sino
					Si character < "0"  o character>"9" Entonces
						valido <- Falso
					FinSi
				FinSi
			FinPara
		FinSi
		
		// Evitar punto al principio o final
		Si SubCadena(capacidad_planta, 1, 1) = "." O SubCadena(capacidad_planta, Longitud(capacidad_planta), Longitud(capacidad_planta)) = "." Entonces
			valido <- Falso
		FinSi
		Si No valido Entonces
			Escribir "Capacidad inválida. Ingrese un número positivo, con o sin decimales (use punto, no coma)."
		FinSi
		
	Hasta Que valido=Verdadero
	capacidad_planta_numero=ConvertirANumero(capacidad_planta)
	
	//TIEMPO DE FUNCIONALIDAD DE LA PLANTA
	Repetir
		valido=Verdadero
		Escribir "Ingrese el número total de meses de operación de la planta eléctrica hasta la fecha actual:"
		Leer funcionamiento_meses
		Si Longitud(funcionamiento_meses) = 0 Entonces
			valido <- Falso
		Sino
            Para i<- 1 Hasta Longitud(funcionamiento_meses)
                character <- SubCadena(funcionamiento_meses, i, i)
                Si character < "0" O character > "9" Entonces
                    valido<- Falso
                FinSi
            FinPara
        FinSi
		Si No valido Entonces
            Escribir "Entrada no válida. Ingrese solo números enteros positivos, sin decimales ni símbolos."
        FinSi
	Hasta Que valido=Verdadero
	numero_mes=ConvertirANumero(funcionamiento_meses)
	
	//CÁLCULOS
	//Se calcula la eficiencia de la planta(restándole un 80% debido a factores como pérdidas por instalación, eficiencia de paneles e inversores, entre otros.
	//La eficiencia obtenida se multiplica por las horas solares promedio en que un panel produce energía (4.5 horas estimadas)
	//Se multiplica lo obtenido para encontrar la producción solar en un mes
	//Se definen los limites(superiores e inferiores) para posteriormente, utilizar la Función random
	//Para definir los límites, se toma en cuenta una desviación estándar del 5%, es decir, que los demás datos de produccion mensual pueden variar en un 5% aproximadamente, basados en el valor central(producción en el mes, que lo tomamos como promedio de producción mensual)
	//Se multiplica la producción solar en un mes por (1-0.05) para límite inferior, y por (1+0.05) para el superior
	//El "1" representa el 100% del valor actual, y como le queremos restar 5%, aplicamos "(1-0.05), lo mismo para el límite superior
	//De esta forma obtenemos valores lógicos generados por random, que mostrarán la producción estimada de cada mes, en función de la capacidad de la planta.
	eficiencia=capacidad_planta_numero*0.8
	horas_solares=4.5
	produccion_dia=eficiencia*horas_solares
	produccion_mes=produccion_dia*30
	Dimensionar mes[numero_mes]
	//Creamos un arreglo para aguardar la produccion mensual
	produccion_acumulada_total=0
	para i=1 hasta numero_mes
		produccion_mensual=Aleatorio((produccion_mes*0.95),(produccion_mes*1.05))
		mes[i]=produccion_mensual
		produccion_acumulada_total=produccion_acumulada_total+produccion_mensual
		
	FinPara
	
	// Mostrar en el resultado
	Para j<-1 Hasta numero_mes Hacer
		Escribir "Mes ",j," : ",mes[j]
	Fin Para
	//
	//Produccion acumulada
	Escribir produccion_acumulada_total
	//facturacion del ultimo mes
	facturar=mes[numero_mes]*precio_kw
	Escribir facturar
	//Facturación acumulada total
	Fac_acumulada_total=produccion_acumulada_total*precio_kw
	Escribir Fac_acumulada_total
	//(Lo de abajo es antes de mostrar el total_)
	//Preguntar al usuario si quiere saber la factura acumulada hasta el mes que ingrese
	Repetir
		valido=Verdadero
		Escribir "Desea saber la facturación acumulada hasta cierto mes? (Si/No)"
		Leer Mayusculas(respuesta)
		Si Longitud(respuesta) = 0 Entonces 
			valido <- Falso
		Sino
			Si respuesta<>"SI" O respuesta<>"SÍ" o respuesta<>"NO" Entonces
				valido=Falso
			FinSi
		SiNo
			respuesta="SI" o respuesta="SÍ" Entonces
			Escribir "Ingrese el mes que desea consultar: "
			Leer acumulado_mes_n
			Si Longitud(acumulado_mes_n) = 0 Entonces
				valido <- Falso
			Sino
				Para i<- 1 Hasta Longitud(acumulado_mes_n)
					character <- SubCadena(acumulado_mes_n, i, i)
					Si character < "0" O character > "9" Entonces
						valido<- Falso
					FinSi
				FinPara
			FinSi
			
		FinSi
	
		
	Hasta Que valido=Verdadero
	
	
	
	
	

	
FinAlgoritmo
