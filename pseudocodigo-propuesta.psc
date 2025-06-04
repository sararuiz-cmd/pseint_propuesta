Algoritmo propuesta_final
	Definir nombre_cliente, direccion, letra Como Caracter
	Definir consumo_anual, consumo_mensual, suma, precio_kw, monto_total,consumo_mensual_actual,gasto_mensual_actual Como Real
	Definir valido Como Logico
	Definir mes,numero_mes Como Entero
	
	suma <- 0
	precio_kw <- 0.13
	
	Escribir "Algoritmo para calcular el consumo mensual de energía en dólares"
	
	// VALIDACIÓN DE DATOS PERSONALES
	Repetir
		valido <- Verdadero
		
		Escribir "Ingrese el nombre del cliente:"
		Leer nombre_cliente
		
		Si Longitud(nombre_cliente) = 0 Entonces
			valido <- Falso
		Sino
			Para i <- 1 Hasta Longitud(nombre_cliente)
				letra <- Subcadena(nombre_cliente, i, i)
				Si No ((letra >= "A" Y letra <= "Z") O (letra >= "a" Y letra <= "z") O letra = "á" O letra = "é" O letra = "í" O letra = "ó" O letra = "ú" O letra = "ñ" O letra = "Ñ") Entonces
					valido <- Falso
				FinSi
			FinPara
		FinSi
		
		Escribir "Ingrese la ubicación del cliente:"
		Leer direccion
		
		Si Longitud(direccion) = 0 Entonces
			valido <- Falso
		Sino
			Para i <- 1 Hasta Longitud(direccion)
				letra <- Subcadena(direccion, i, i)
				Si No ((letra >= "A" Y letra <= "Z") O (letra >= "a" Y letra <= "z") O letra = "á" O letra = "é" O letra = "í" O letra = "ó" O letra = "ú" O letra = "ñ" O letra = "Ñ" O letra = " ") Entonces
					valido <- Falso
				FinSi
			FinPara
		FinSi
		
		Si No valido Entonces
			Escribir "Error: Nombre o dirección inválidos. Solo letras sin números ni símbolos."
		FinSi
	Hasta Que valido = Verdadero
	
	// ENTRADA DE GASTOS MENSUALES PASADOS
	Escribir "Cálculo del consumo anual del año pasado"
	Para mes <- 1 Hasta 12
		Repetir
			valido <- Verdadero
			Escribir "Ingrese el gasto del mes ", mes, " en dólares:"
			Leer consumo_mensual
			
			Si consumo_mensual <= 0 Entonces
				valido <- Falso
				Escribir "El valor debe ser numérico y mayor que cero."
			FinSi
		Hasta Que valido = Verdadero
		
		suma <- suma + consumo_mensual
	FinPara
	
	// CÁLCULOS
	consumo_anual <- suma / precio_kw
	//CÁLCULO DEL GASTO DE UN DETERMINADO MES
	//VALIDACIÓN DE ENTRADAS
	acumulacion=0 //INICIALIZAR CONTADOR PARA SUMAR LOS GASTOS DE CADA MES
	Repetir
		valido=Verdadero
		Escribir "Ingrese el mes (en números) en que desea calcular el gasto de energía"
		leer numero_mes
		si numero_mes<0 o numero_mes>=12 Entonces
			valido=Falso
			Escribir "El valor debe ser numérico y estar en el rango (1-12)"
		FinSi
	Hasta Que valido=Verdadero
	Para j<-1 Hasta numero_mes Con Paso +1 Hacer
		Repetir
			valido=Verdadero
			Escribir "Ingrese el gasto del mes ",j
			Leer consumo_mensual_actual
			si consumo_mensual_actual<0 Entonces
				valido=Falso
				Escribir "El valor debe ser positivo"
			FinSi
		Hasta Que valido=Verdadero
		acumulacion=acumulacion+consumo_mensual_actual
	Fin Para
	gasto_mensual_actual=consumo_anual-acumulacion
	
	
	
	

FinAlgoritmo
