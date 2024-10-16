Algoritmo algor
	Definir resp2 como cadena
	DEFINIR NOM, APP, APM,NOMC1, LUGARN,RESP1,SEXO COMO CADENA 
	Definir AP1,AP11,APMYN, FNAC,CONAPP,CONAPM,CONN,ENTI Como CADENA
	Definir  DIA, MES, AÑO, LONG, i, j, k, r, s,v1,v2,v3,v4,v5,VAN,VEN,VIN,VON,VUN,VAPP,VEAPP,VIAPP,VOAPP,VUAPP,VAAPM,VEAPM,VIAPM,VOAPM,VUAPM Como Entero
	Mientras resp2 <> 'n' Hacer
		Limpiar Pantalla
		Escribir '----------------------------------------------------------------------'
		Escribir 'BIENVENIDO'
		Escribir 'INGRESE SU NOMBRE(S)'
		Leer  NOM
		Escribir 'INGRESE APELLIDO PATERNO'
		LEER APP
		Escribir 'INGRESE APELLIDO MATERNO'
		LEER APM	
		Escribir 'SEXO:'
		Escribir 'H (hombre)'
		Escribir 'M (mujer)'
		Leer SEXO
		Escribir '----------------------------------------------------------------------'
		ESCRIBIR 'SU NOMBRE ES ' Mayusculas(NOM) ' ' Mayusculas(APP) ' ' Mayusculas(APM)
		Escribir '----------------------------------------------------------------------'
		ESCRIBIR 'SU NOMBRE ES ' Minusculas(NOM) ' ' Minusculas(APP) ' ' Minusculas(APM)
		Escribir '----------------------------------------------------------------------'
		Escribir 'A CONTINUACION INGRESE LA FECHA DE SU NACIMIENTO'
		Escribir 'INGRESE EL DIA '
		Leer DIA
		ESCRIBIR 'INGRESE EL MES'
		LEER MES
		Escribir  'AÑO(año con 4 digitos)'
		LEER AÑO
		Repetir
			Escribir 'LUGAR DE NACIMIENTO'
			Escribir 'A) CDMX'
			Escribir 'B) EDOMEX'
			LEER RESP1
			RESP1 <- Minusculas(RESP1)
		Hasta Que RESP1 == 'a' o RESP1 == 'b'
		SI (RESP1 == 'a') Entonces
			LUGARN <- 'CDMX'
		SiNo
			LUGARN <- 'EDOMEX'
		FinSi
		Escribir '----------------------------------------------------------------------'
		Escribir 'USTED NACIO EL ' DIA '/' MES '/' AÑO ' EN ' LUGARN
		Escribir '----------------------------------------------------------------------'
		LONG <- Longitud(NOM) + Longitud(APP) + Longitud(APM)
		Escribir LONG ' ES LA CANTIDAD DE LETRAS QUE TIENE SU NOMBRE COMPLETO'
		APP <- Minusculas(APP)
		APM <- Minusculas(APM)
		NOM <- Minusculas(NOM)
		Dimension NOM1[Longitud(NOM)]
		Dimension APP1[Longitud(APP)]
		Dimension APM1[Longitud(APM)]
		//SEPARANDO LETRA POR LETRA
		Para i <- 1 Hasta Longitud(NOM) Con Paso 1 Hacer
			NOM1[i] <- Subcadena(NOM,i,i)
		Fin Para
		Para k <- 1 Hasta Longitud(APP) Con Paso 1 Hacer
			APP1[k] <- Subcadena(APP,k,k)
		Fin Para
		Para s <- 1 Hasta Longitud(APM) Con Paso 1 Hacer
			APM1[s] <- Subcadena(APM,s,s)
		Fin Para
		//CONTANDO VOCALES
		k <- 0
		i<-0
		s<-0
		//VOCALES DEL NOMBRE 
		Mientras i < Longitud(NOM) Hacer
			i <- i+1
			Segun NOM1[i] Hacer
				'a':
					v1 <- v1 + 1
				'e':
					v2 <- v2 + 1
				'i':
					v3 <- v3 +1
				'o':
					v4 <- v4 +1
				'u':
					v5 <- v5 +1
			Fin Segun
			VAN <- v1
			VEN <- v2
			VIN <- v3
			VON <- v4
			VUN <- v5
		Fin Mientras
		v1 <- 0
		v2 <- 0
		v3 <- 0
		v4 <- 0
		v5 <- 0
		//VOCALES DEL APPELIDO PATERNO 
		Mientras k < Longitud(APP) Hacer
			k <- k+1
			Segun APP1[k] Hacer
				'a':
					v1 <- v1 + 1
				'e':
					v2 <- v2 + 1
				'i':
					v3 <- v3 +1
				'o':
					v4 <- v4 +1
				'u':
					v5 <- v5 +1
			Fin Segun
			VAPP <- v1
			VEAPP <- v2
			VIAPP <- v3
			VOAPP <- v4
			VUAPP <- v5
		FinMientras
		v1 <- 0
		v2 <- 0
		v3 <- 0
		v4 <- 0
		v5 <- 0
		// VOCALES DE APM
		Mientras s < Longitud(APM) Hacer
			s <- s+1
			Segun APM1[s] Hacer
				'a':
					v1 <- v1 + 1
				'e':
					v2 <- v2 + 1
				'i':
					v3 <- v3 +1
				'o':
					v4 <- v4 +1
				'u':
					v5 <- v5 +1
			Fin Segun
			VAAPM <- v1
			VEAPM <- v2
			VIAPM <- v3
			VOAPM <- v4
			VUAPM <- v5
		FinMientras
		//Imprimir DATOS
		Escribir '----------------------------------------------------------------------'
		Escribir 'LA CANTIDAD DE VOCALES EN SU NOMBRE ES:  a-' VAN ' e-' VEN ' i-' VIN ' o-' VON ' u-' VUN
		Escribir 'LA CANTIDAD DE VOCALES EN SU APELLIDO PATERNO ES:  a-' VAPP ' e-' VEAPP ' i-' VIAPP ' o-' VOAPP ' u-' VUAPP
		Escribir 'LA CANTIDAD DE VOCALES EN SU APELLIDO MATERNO ES:  a-' VAAPM ' e-' VEAPM ' i-' VIAPM ' o-' VOAPM ' u-' 	VUAPM
		Escribir '----------------------------------------------------------------------'
		k <- 0
		i<-0
		s<-0
		//PRIMERA VOCAL DEL 1ER APELLIDO
		Para i<-2 Hasta Longitud(APP) Con Paso 1 Hacer
			si APP1[i] == 'a' o APP1[i] == 'e' o APP1[i] == 'i' o APP1[i] == 'o' o APP1[i] == 'u' Entonces
				AP1 <- APP1[i]
				i <- Longitud(APP)
			FinSi
		Fin Para
		//CONSONONTES DE LOS NOMBRES
		Para j<-2 Hasta Longitud(APM) Con Paso 1 Hacer
			si APM1[j] <> 'a' Y APM1[j] <> 'e' Y APM1[j] <> 'i' Y APM1[j] <> 'o' Y APM1[j] <> 'u' Entonces
				CONAPM <- APM1[j]
				si Mayusculas(CONAPM) == 'Ñ' Entonces
					CONAPM <- 'X'
				FinSi
				j <- Longitud(APM)
			FinSi
		Fin Para
		Para k<-2 Hasta Longitud(NOM) Con Paso 1 Hacer
			si NOM1[k] <> 'a' Y NOM1[k] <> 'e' Y NOM1[k] <> 'i' Y NOM1[k] <> 'o' Y NOM1[k] <> 'u' Entonces
				CONN <- NOM1[k]
				SI Mayusculas(CONN) == 'Ñ' Entonces
					CONN <- ''
				FinSi
				k <- Longitud(NOM)
			FinSi
		Fin Para
		Para s<-2 Hasta Longitud(APP) Con Paso 1 Hacer
			si APP1[s] <> 'a' Y APP1[s] <> 'e' Y APP1[s] <> 'i' Y APP1[s] <> 'o' Y APP1[s] <> 'u' Entonces
				CONAPP <- APP1[s]
				SI Mayusculas(CONAPP) == 'Ñ' Entonces
					CONAPP <- 'X'
				FinSi
				s <- Longitud(APP)
			FinSi
		Fin Para
		//CLAVE DE ENTIDAD
		SI LUGARN == 'CDMX' Entonces
			ENTI <- 'DF'
		SiNo
			ENTI <- 'MC'
		FinSi
		//CURP PRIMEROS PASOS
		FNAC <- ConvertirATexto(AÑO)
		AP11 <- Concatenar(Subcadena(APP,1,1),AP1)
		APMYN <- Concatenar(Subcadena(APM,1,1),Subcadena(NOM,1,1))
		APMYN <- Concatenar(AP11,APMYN)
		//COMRPOBACION VOCALES Y CONSONANTES
		Segun Mayusculas(APMYN) Hacer
			'LOCA':
				APMYN <- 'LXCA'
			'VACA':
				APMYN <- 'VXCA'
			'COLA':
				APMYN <- 'CXLA'
			De Otro Modo:
				APMYN <- APMYN 
		Fin Segun
		APMYN <- Concatenar(APMYN, Subcadena(FNAC,3,4))
		APMYN <- Concatenar(APMYN , ConvertirATexto(0))
		APMYN <- Concatenar(APMYN , ConvertirATexto(MES))
		APMYN <- Concatenar(APMYN , ConvertirATexto(DIA))
		APMYN <- Concatenar(APMYN , SEXO)
		APMYN <- Concatenar(APMYN , ENTI)
		APMYN <- Concatenar(APMYN , CONAPP)
		APMYN <- Concatenar(APMYN , CONAPM)
		APMYN <- Concatenar(APMYN , CONN)
		APMYN <- Concatenar(APMYN , ConvertirATexto(0))
		APMYN <- Concatenar(APMYN , ConvertirATexto(1))
		Escribir 'SU CURP ES: ' Mayusculas(APMYN)
		Escribir '----------------------------------------------------------------------'
		Escribir 'DESEA REPETIR LA APLICACION S/N'
		Leer resp2
		resp2 <- Minusculas(resp2)
	Fin Mientras
FinAlgoritmo