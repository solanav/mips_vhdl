Forwarding interno en el banco de registros:
Para solucionar este problema nosotros hemos optado por la solucion
que consiste en hacer que el banco de registros funcione en flanco de
bajada. Esto soluciona el problema porque el banco puede leer los 
datos que le llegan en el flanco de subida y escribirlos en el de bajada,
de tal forma que al siguiente flanco de subida los datos ya estan
guardados en el banco de registros y las instrucciones operaran con
los valores correctos.
