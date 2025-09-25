# BATCH_FW - Batch Framework Subsystem

## Especificaciones

### Propósito
El subsistema BATCH_FW proporciona un framework completo para el procesamiento batch en el sistema ACAS. Gestiona la carga de datos batch desde archivos COBOL hacia tablas relacionales (RDB) y viceversa, con soporte para procesamiento de transacciones y control de errores.

### Componentes Principales

1. **glbatchLD.cbl** - Cargador de Batch (Batch Loader)
   - Version: 3.02.15
   - Carga datos desde archivos GL Batch hacia tablas MySQL
   - Soporta commit/rollback transaccional
   - Maneja duplicados con lógica de reescritura

2. **glbatchUNL.cbl** - Descargador de Batch (Batch Unloader)  
   - Version: 3.02.00
   - Extrae datos desde archivos ISAM a archivos secuenciales (.seq)
   - Funciona como backup/respaldo
   - Salida de registros va a SYS-DISPLAY.log

3. **glbatchMT.cbl** - Manejador de Tablas (Table Handler)
   - Version: 3.02.11
   - Interfaz RDB completa para operaciones CRUD
   - Implementa DAL (Data Access Layer) para MySQL
   - Gestión de cursores y transacciones

4. **glbatchRES.cbl** - Restaurador de Batch
   - Restaura archivos desde backups .seq

## Interfaces

### Interfaces de Archivos
- **Archivo Batch COBOL**: Archivo ISAM indexado
- **Tabla GL Batch RDB**: Tabla MySQL con estructura equivalente
- **Archivo Secuencial .seq**: Formato de backup

### Campos de Datos Principales
```cobol
01 WS-Batch-Record.
   03 BATCH-KEY          PIC 9(8).
   03 ITEMS              PIC 999.
   03 BATCH-STATUS       PIC 999.
   03 CLEARED-STATUS     PIC 999.
   03 BCYCLE             PIC 999.
   03 ENTERED            PIC 9(8).
   03 PROOFED            PIC 9(8).
   03 POSTED             PIC 9(8).
   03 STORED             PIC 9(8).
   03 INPUT-GROSS        PIC S9(11)V99.
   03 INPUT-VAT          PIC S9(11)V99.
   03 ACTUAL-GROSS       PIC S9(11)V99.
   03 ACTUAL-VAT         PIC S9(11)V99.
   03 DESCRIPTION        PIC X(24).
   03 BDEFAULT           PIC 999.
   03 CONVENTION         PIC XX.
   03 BATCH-DEF-AC       PIC 9(6).
   03 BATCH-DEF-PC       PIC 999.
   03 BATCH-DEF-CODE     PIC XX.
   03 BATCH-DEF-VAT      PIC X.
   03 BATCH-START        PIC 9(6).
```

### APIs y Llamadas
- **acas007**: Interfaz principal para acceso a archivos COBOL
- **acas-get-params**: Obtiene parámetros RDB desde acas.param
- **fhlogger**: Módulo de logging para auditoría
- **ACAS-Sysout**: Módulo de salida del sistema
- **MySQL_***: APIs nativas de MySQL para operaciones RDB

## Flujos de Proceso

### Flujo de Carga (Load)
1. Abrir archivo System para obtener configuración RDB
2. Validar configuración de base de datos
3. Abrir archivo Batch COBOL (input)
4. Inicializar conexión RDB (MySQL)
5. Por cada registro:
   - Leer registro COBOL
   - Convertir a formato RDB
   - Intentar INSERT
   - Si duplicado, hacer UPDATE
   - Registrar en log
6. Cerrar conexiones y reportar estadísticas

### Flujo de Descarga (Unload)
1. Abrir archivo Batch COBOL
2. Crear archivo secuencial .seq
3. Copiar todos los registros
4. Cerrar archivos
5. Reportar contadores

### Flujo de Manejo RDB
1. **OPEN**: Inicializar conexión MySQL
2. **READ-NEXT**: SELECT con cursor y FETCH iterativo
3. **READ-INDEXED**: SELECT directo por clave
4. **WRITE**: INSERT con manejo de duplicados
5. **REWRITE**: UPDATE por clave primaria
6. **DELETE**: DELETE individual o DELETE-ALL
7. **START**: Posicionar cursor con condición
8. **CLOSE**: Liberar recursos y cerrar conexión

## Dependencias

### Módulos Requeridos
- acas007 - Manejador de archivos COBOL
- acas-get-params - Lector de configuración
- fhlogger - Sistema de logging
- ACAS-Sysout - Salida del sistema
- MySQL libraries - Bibliotecas cliente MySQL

### Archivos de Configuración
- system.dat - Archivo de sistema principal
- acas.param - Parámetros de conexión RDB

### Copybooks Utilizados
- wsbatch.cob - Definición de registro Batch
- wsfnctn.cob - Definiciones de funciones
- wsnames.cob - Nombres del sistema
- wscall.cob - Definiciones de llamadas
- envdiv.cob - División de entorno
- Test-Data-Flags.cob - Flags de prueba
- mysql-variables.cpy - Variables MySQL
- mysql-procedures.cpy - Procedimientos MySQL

## Manejo de Errores

### Códigos de Error del Sistema
- SY001: Error de lectura del sistema
- SY004: Problema abriendo archivo sistema
- SY006: Argumentos de programa incorrectos
- SY007: Argumentos incorrectos
- SY008: Mensaje de nota
- SY009: Variables de entorno no configuradas
- SY019: RDB no configurado en archivo param

### Códigos de Error GL Batch
- GL010: Claves duplicadas encontradas
- GL011: Error en procesamiento glbatchMT
- GL012: Error abriendo archivo Batch
- GL013: Error leyendo archivo
- GL014: Error escribiendo tabla
- GL015: Error abriendo tabla GLBatch
- GL018: Detalles RDBMS
- GL097: No existe archivo Batch
- GL098: Reescribiendo
- GL099: Ignorando

### Estados de Respuesta RDB
- 0: Operación exitosa
- 10: Fin de archivo (EOF)
- 21: Clave inválida o no encontrada
- 22: Intento de duplicar clave
- 23: Clave no encontrada
- 99: Error genérico (ver WE-Error)

### Códigos WE-Error Específicos
- 901: Tamaño de registro incorrecto
- 910: Tabla bloqueada > 5 segundos
- 911: Error RDB durante inicialización
- 989: Error inesperado en Read-Indexed
- 990: Error desconocido e inesperado
- 992: Función inválida solicitada
- 994: Error durante Rewrite
- 995: Error durante Delete
- 996: Clave de borrado fuera de rango
- 997: Access-Type incorrecto
- 998: File-Key-No fuera de rango

## Notas de Implementación

### Consideraciones de Rendimiento
- Usa cursores para lecturas secuenciales masivas
- Implementa batch commits para cargas grandes
- El autocommit debe estar OFF en MySQL para transacciones

### Seguridad
- Credenciales RDB almacenadas en acas.param
- Logging completo de todas las operaciones
- Control de acceso mediante sistema de archivos

### Compatibilidad
- Soporta múltiples RDBMs mediante DAL específicos
- Compatible con ISAM tradicional y RDB moderno
- Permite procesamiento dual (archivo + RDB)

### Limitaciones
- Requiere MySQL con autocommit=OFF para transacciones
- Los archivos de log pueden crecer rápidamente
- No soporta operaciones interactivas con flags -i