# SEC_AUDIT - Security and Audit Subsystem

## Especificaciones

### Propósito
El subsistema SEC_AUDIT proporciona capacidades completas de auditoría y seguridad para el sistema ACAS. Registra todas las transacciones de inventario, movimientos de stock y cambios de valor, manteniendo un rastro de auditoría completo para cumplimiento y análisis.

### Componentes Principales

1. **auditLD.cbl** - Cargador de Auditoría
   - Version: 3.02.12
   - Carga registros de auditoría desde archivos hacia tablas MySQL
   - Maneja transacciones con commit/rollback
   - Procesa duplicados mediante reescritura

2. **auditMT.cbl** - Manejador de Tablas de Auditoría
   - Version: 3.02.13
   - Interfaz DAL completa para operaciones CRUD
   - Gestión de cursores y búsquedas
   - Soporte para eliminación masiva (DELETE-ALL)

3. **auditUNL.cbl** - Descargador de Auditoría
   - Extrae registros a archivos secuenciales de respaldo

4. **auditRES.cbl** - Restaurador de Auditoría
   - Restaura registros desde backups

## Interfaces

### Estructura de Registro de Auditoría
```cobol
01 STOCKAUDIT-REC.
   03 WS-Audit-ID               PIC 9(8).      *> ID único para RDBMS
   03 Audit-Key.
      05 Audit-Type             PIC 9.
      05 Audit-Stock-Key        PIC X(13).
   03 Audit-Invoice-PO          PIC 9(8).
   03 Audit-Cr-for-Invoice      PIC 9(8).
   03 Audit-Desc                PIC X(32).
   03 Audit-Process-Date        PIC X(10).
   03 Audit-Reverse-Transaction PIC 9.          *> T/F (1/0)
   03 Audit-Transaction-Qty     PIC S9(6).
   03 Audit-Unit-Cost           PIC S9(6)V9999.
   03 Audit-Stock-Value-Change  PIC S9(8)V99.   *> Puede ser negativo
   03 Audit-No                  PIC 9(5).
```

### Campos de Auditoría Capturados
- **Identificación**: ID único y clave compuesta (tipo + stock key)
- **Transacción**: Número de factura/PO y referencia de crédito
- **Descripción**: Texto descriptivo de 32 caracteres
- **Fecha**: Fecha de procesamiento en formato X(10)
- **Reverso**: Indicador de transacción reversa
- **Cantidades**: Cantidad de transacción (con signo)
- **Valores**: Costo unitario y cambio de valor de stock
- **Número**: Número de auditoría secuencial

### APIs y Módulos Llamados
- **acas010**: Manejador de archivos de auditoría COBOL
- **MySQL_***: APIs nativas para operaciones de base de datos
- **fhlogger**: Sistema de logging para auditoría
- **ACAS-Sysout**: Módulo de salida del sistema
- **acas-get-params**: Obtención de parámetros RDB

## Flujos de Proceso

### Flujo de Registro de Auditoría
1. Captura de evento transaccional
2. Generación de registro de auditoría con:
   - Tipo de transacción
   - Clave de stock afectado
   - Valores monetarios
   - Cantidades
   - Fecha/hora
3. Escritura en archivo/tabla de auditoría
4. Verificación de integridad

### Flujo de Carga de Auditoría (Load)
1. Apertura de archivo sistema para configuración
2. Validación de parámetros RDB
3. Apertura de archivo de auditoría COBOL
4. Inicialización de conexión MySQL
5. Procesamiento registro por registro:
   - Lectura de archivo
   - Conversión a formato RDB
   - INSERT con manejo de duplicados
   - UPDATE si existe
6. Cierre y reporte de estadísticas

### Flujo de Consulta de Auditoría
1. **Búsqueda Secuencial**: 
   - SELECT con ORDER BY AUDIT-ID ASC
   - FETCH iterativo con cursor
2. **Búsqueda Indexada**:
   - SELECT directo por AUDIT-ID
3. **Búsqueda Condicional (START)**:
   - Soporta operadores: =, <, >, >=, <=
   - Posicionamiento de cursor

### Flujo de Mantenimiento
1. **DELETE Individual**: Por AUDIT-ID específico
2. **DELETE-ALL**: Eliminación masiva hasta ID 99999999
3. **REWRITE**: Actualización de registros existentes

## Dependencias

### Módulos del Sistema
- acas010 - Manejador de archivos de auditoría
- System-File - Archivo de configuración del sistema
- MySQL client libraries - Bibliotecas cliente MySQL

### Archivos de Configuración
- system.dat - Configuración principal
- acas.param - Parámetros de conexión RDB

### Copybooks Requeridos
- wsaudit.cob - Definición de registro de auditoría
- wsfnctn.cob - Definiciones de funciones
- wsnames.cob - Nombres del sistema
- wscall.cob - Definiciones de llamadas
- Test-Data-Flags.cob - Flags de prueba
- mysql-variables.cpy - Variables MySQL
- mysql-procedures.cpy - Procedimientos MySQL
- ACAS-SQLstate-error-list.cob - Lista de errores SQL

## Manejo de Errores

### Códigos de Error del Sistema
- SY001: Error de lectura del sistema
- SY004: Problema con archivo sistema
- SY006: Argumentos de programa limitados
- SY007: Argumentos incorrectos
- SY008: Mensaje de nota
- SY009: Variables de entorno no configuradas
- SY019: DB no configurada en archivo param

### Códigos de Error de Auditoría (SL)
- SL010: Claves duplicadas encontradas
- SL011: Error en procesamiento auditMT
- SL012: Error abriendo archivo Audit
- SL013: Error leyendo archivo
- SL014: Error escribiendo tabla
- SL015: Error abriendo tabla Audit
- SL018: Detalles RDBMS
- SL097: No existe archivo Audit
- SL099: Ignorando error

### Estados SQL y Manejo
- "00000": Operación exitosa
- "23000": Violación de clave duplicada
- Otros estados según estándar ANSI SQL

### Códigos WE-Error
- 989: Error inesperado en Read-Indexed
- 990: Error desconocido
- 992: Función inválida
- 994: Error durante Rewrite
- 995: Error durante Delete
- 997: Access-Type incorrecto
- 910: Tabla bloqueada
- 911: Error de inicialización RDB

## Consideraciones de Seguridad

### Control de Acceso
- Acceso controlado mediante permisos del sistema de archivos
- Credenciales RDB protegidas en acas.param
- Sin modificación permitida de registros históricos

### Integridad de Datos
- Transacciones con soporte commit/rollback
- Verificación de duplicados antes de inserción
- Validación de tipos de datos en conversión

### Rastro de Auditoría
- Registro completo de todas las operaciones
- Timestamps en cada transacción
- Capacidad de reverso de transacciones marcada

### Cumplimiento
- Retención permanente de registros
- No permite eliminación selectiva (solo masiva)
- Formato estructurado para reportes regulatorios

## Notas de Implementación

### Rendimiento
- Índice primario en AUDIT-ID para búsquedas rápidas
- Cursores para procesamiento masivo eficiente
- Batch processing para cargas grandes

### Almacenamiento
- Crecimiento continuo de datos de auditoría
- Considerar particionamiento por fecha
- Archivado periódico recomendado

### Monitoreo
- Logs de operaciones en fhlogger
- Estadísticas de carga/proceso
- Alertas en errores críticos

### Limitaciones
- ID de auditoría limitado a 8 dígitos
- Descripción limitada a 32 caracteres
- Sin soporte para auditoría en tiempo real (batch only)