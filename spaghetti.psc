Algoritmo propuesta_final
	Definir nombre_cliente, direccion, letra,funcionamiento_meses, produccion_mensual_entrada,capacidad_planta,respuesta,acumulado_mes_n Como Caracter
	Definir precio_kw Como Real
	Definir valido, salir Como Logico
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
				Si No ((letra >= "A" Y letra <= "Z") O (letra >= "a" Y letra <= "z") O letra = "á" O letra = "é" O letra = "í" O letra = "ó" O letra = "ú" O letra = "ñ" O letra = "Ñ" O letra = " ") Entonces
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
	eficiencia=capacidad_planta_numero*0.2
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
	
	mes_calculado = ((numero_mes - 1) Mod 12) + 1  // Esto ajusta el mes para que esté dentro del rango 1-12
	
	Segun mes_calculado Hacer
		1: nombreMes = "Enero"
		2: nombreMes = "Febrero"
		3: nombreMes = "Marzo"
		4: nombreMes = "Abril"
		5: nombreMes = "Mayo"
		6: nombreMes = "Junio"
		7: nombreMes = "Julio"
		8: nombreMes = "Agosto"
		9: nombreMes = "Septiembre"
		10: nombreMes = "Octubre"
		11: nombreMes = "Noviembre"
		12: nombreMes = "Diciembre"
	Fin Segun
	
	mes_anterior <- ((numero_mes - 2 + 12) Mod 12) + 1 //para mostrar el anterior (tu culpa sara >:B)
	Segun mes_anterior Hacer
		1: nombre = "Enero"
		2: nombre = "Febrero"
		3: nombre = "Marzo"
		4: nombre = "Abril"
		5: nombre = "Mayo"
		6: nombre = "Junio"
		7: nombre = "Julio"
		8: nombre = "Agosto"
		9: nombre = "Septiembre"
		10: nombre = "Octubre"
		11: nombre = "Noviembre"
		12: nombre = "Diciembre"
	Fin Segun
	
	
	Fac_acumulada_total=produccion_acumulada_total*precio_kw//la suma de toda la produccion por el precio
	
	Escribir "           "
	Escribir "           "
	Escribir "           "
	Escribir "           "
	
	Escribir 'Cliente :' , nombre_cliente
	
	Escribir "           "
	
	Escribir "           "
	
	Escribir "           "
	
	
	Escribir "    Ubicación : ", direccion
	
	Escribir "           "
	
	Escribir '    Capacidad: ', capacidad_planta, ' kwp'
	
	Escribir "           "
	
	Escribir "           "
	
	Escribir '    Detalles de producción: '
	Escribir "           "
	Escribir "           "
	Escribir "                      Produccción acumulada hasta ", nombreMes ,'  :   ',produccion_acumulada_total
	Escribir "                      Corte                       ", nombre ,'     : ',produccion_acumulada_total - mes[numero_mes]
	Escribir "                      Producción Mes Actual       ' , nombreMes, ' : ' ,mes[numero_mes] ,' kw'
	Escribir "                      A facturar ", mes[numero_mes] , ' * $0.13       ' ' :      ' '$' mes[numero_mes] * 0.13  
	Escribir '                      la facturación acumulada hasta ',nombreMes, ': ' , '$', Fac_acumulada_total
	
	
	Escribir "           "
	Escribir '                                                                    este valor no incluye IVA. '
	Escribir "           "
	
	Escribir "           "
	
	Repetir
		valido <- Verdadero
		
		Escribir "Desea saber la facturación acumulada hasta cierto mes? (Si/No)"
		Leer respuesta
		respuesta <- Mayusculas(respuesta)
		
		Si Longitud(respuesta) = 0 Entonces
			valido <- Falso
		Sino
			Si respuesta <> "SI" Y respuesta <> "SÍ" Y respuesta <> "NO" Entonces
				valido <- Falso
			FinSi
		FinSi
		
		Si respuesta = "SI" O respuesta = "SÍ" Entonces
			Escribir "Ingrese el mes que desea consultar(solo número del mes): "
			Leer acumulado_mes_n
			
			Si Longitud(acumulado_mes_n) = 0 Entonces
				valido <- Falso
			Sino
				Para i <- 1 Hasta Longitud(acumulado_mes_n)
					character <- SubCadena(acumulado_mes_n, i, i)
					Si No (character >= "0" Y character <= "9") Entonces
						valido <- Falso
					FinSi
				FinPara
			FinSi
			
			monthly_accumulation_n <- ConvertirANumero(acumulado_mes_n) //e
			accumulated_bill <- 0
			i <- 1
			
			Mientras i <= numero_mes Y i <= monthly_accumulation_n Hacer
				accumulated_bill <- accumulated_bill + mes[i]
				i <- i + 1
			FinMientras
			
			mes_nombre_1 = ((monthly_accumulation_n - 1) Mod 12) + 1  // Esto ajusta el mes para que esté dentro del rango 1-12
			
			Segun mes_nombre_1 Hacer
				1: nombre_Mes = "Enero"
				2: nombre_Mes = "Febrero"
				3: nombre_Mes = "Marzo"
				4: nombre_Mes = "Abril"
				5: nombre_Mes = "Mayo"
				6: nombre_Mes = "Junio"
				7: nombre_Mes = "Julio"
				8: nombre_Mes = "Agosto"
				9: nombre_Mes = "Septiembre"
				10: nombre_Mes = "Octubre"
				11: nombre_Mes = "Noviembre"
				12: nombre_Mes = "Diciembre"
			Fin Segun

			
			Escribir "Facturación acumulada hasta ", nombre_Mes, ": ",'$',accumulated_bill*precio_kw
		FinSi
		
		Si No valido Entonces
			Escribir "Error: Entrada incorrecta. Por favor, intente nuevamente."
		FinSi
		
	Hasta Que valido = Verdadero
	
	
	


	
FinAlgoritmo