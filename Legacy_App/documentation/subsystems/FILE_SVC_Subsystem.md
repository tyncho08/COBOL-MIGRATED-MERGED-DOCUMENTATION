# FILE_SVC - File Services Subsystem

## Especificaciones

### Propósito
El subsistema FILE_SVC proporciona servicios centralizados de manejo de archivos para el sistema ACAS. Ofrece una capa de abstracción unificada para operaciones con archivos COBOL ISAM y tablas relacionales, incluyendo carga (LD), descarga (UNL), restauración (RES) y manejo de tablas (MT).

### Componentes y Patrones

El subsistema sigue un patrón consistente para cada entidad de datos:
- **xxLD.cbl**: Cargador de datos desde archivo a tabla RDB
- **xxUNL.cbl**: Descargador de datos a archivo secuencial
- **xxRES.cbl**: Restaurador desde archivo secuencial
- **xxMT.cbl**: Manejador de tablas RDB (DAL)

### Entidades Gestionadas

1. **Datos Maestros**
   - nominal (Chart of Accounts)
   - system (Configuración del sistema)
   - value (Valores y parámetros)
   - anal (Análisis)

2. **Datos Transaccionales**
   - glposting (Postings GL)
   - irsposting (Postings IRS)
   - slposting (Postings ventas)
   - invoice (Facturas)
   - payments (Pagos)
   - delivery (Entregas)

3. **Datos de Configuración**
   - dflt (Defaults GL/IRS)
   - final (Finales GL/IRS)
   - sys4 (Sistema 4)

4. **Datos de Control**
   - batch (Control de lotes)
   - autogen (Generación automática)
   - delinvnos (Números de factura eliminados)

## Interfaces

### Interfaz Estándar de File-Access
```cobol
01 File-Access.
   03 File-Function        PIC 9.
      88 fn-Open           VALUE 1.
      88 fn-Close          VALUE 2.
      88 fn-Read-Next      VALUE 3.
      88 fn-Read-Indexed   VALUE 4.
      88 fn-Write          VALUE 5.
      88 fn-Delete-All     VALUE 6.
      88 fn-Rewrite        VALUE 7.
      88 fn-Delete         VALUE 8.
      88 fn-Start          VALUE 9.
   03 Access-Type          PIC 9.
      88 fn-input          VALUE 1.
      88 fn-output         VALUE 2.
      88 fn-i-o            VALUE 3.
      88 fn-extend         VALUE 4.
      88 fn-equal-to       VALUE 5.
      88 fn-less-than      VALUE 6.
      88 fn-greater-than   VALUE 7.
      88 fn-not-less-than  VALUE 8.
```

### Interfaz de Respuesta
```cobol
01 ACAS-DAL-Common-Data.
   03 FS-Reply            PIC 99.
   03 WE-Error            PIC 999.
   03 SQL-Err             PIC X(5).
   03 SQL-Msg             PIC X(70).
   03 SQL-State           PIC X(5).
   03 WS-File-Key         PIC X(20).
   03 WS-Log-Where        PIC X(512).
```

### Operaciones Soportadas

1. **OPEN**: Inicializar archivo/tabla
   - Input, Output, I-O, Extend
   - Inicialización de conexión RDB

2. **CLOSE**: Cerrar archivo/tabla
   - Liberar recursos
   - Cerrar conexión RDB

3. **READ-NEXT**: Lectura secuencial
   - Con cursor para RDB
   - Orden por clave primaria

4. **READ-INDEXED**: Lectura directa por clave

5. **WRITE**: Insertar nuevo registro
   - Manejo de duplicados
   - Conversión COBOL a RDB

6. **REWRITE**: Actualizar registro existente

7. **DELETE**: Eliminar registro por clave

8. **DELETE-ALL**: Eliminación masiva

9. **START**: Posicionar para lectura
   - Soporta operadores relacionales
   - Gestión de cursores

## Flujos de Proceso

### Flujo de Carga (LD)
```
1. Leer configuración del sistema
2. Obtener parámetros RDB (acas.param)
3. Abrir archivo fuente COBOL
4. Inicializar conexión RDB
5. Para cada registro:
   - Leer de archivo COBOL
   - Convertir formato
   - INSERT en tabla RDB
   - Si duplicado: UPDATE
   - Registrar en log
6. Commit/Rollback según resultado
7. Cerrar conexiones
8. Reportar estadísticas
```

### Flujo de Descarga (UNL)
```
1. Abrir archivo fuente
2. Crear archivo .seq destino
3. Copiar todos los registros
4. Sin conversión de formato
5. Cerrar archivos
6. Reportar contadores
```

### Flujo de Restauración (RES)
```
1. Leer archivo .seq
2. Validar formato
3. Cargar en archivo ISAM
4. Reconstruir índices
5. Verificar integridad
6. Reportar resultados
```

### Flujo de Manejo RDB (MT)
```
1. Recibir solicitud vía File-Access
2. Determinar operación (File-Function)
3. Ejecutar operación SQL correspondiente
4. Convertir entre formatos COBOL/RDB
5. Gestionar cursores si aplica
6. Retornar resultado y códigos de error
```

## Dependencias

### Módulos Centrales
- acas001-acas015: Manejadores específicos por archivo
- fhlogger: Sistema de logging
- ACAS-Sysout: Salida del sistema
- acas-get-params: Configuración RDB

### Archivos de Sistema
- system.dat: Configuración principal
- acas.param: Parámetros de conexión RDB

### Bibliotecas Externas
- MySQL client libraries
- GnuCOBOL runtime
- ISAM file handlers

## Manejo de Errores

### Códigos de Retorno Estándar
- 00: Operación exitosa
- 10: Fin de archivo (EOF)
- 21: Clave inválida en START
- 22: Intento de duplicar clave
- 23: Clave no encontrada
- 35: Archivo no existe
- 99: Error genérico

### Manejo de Transacciones
- Autocommit debe estar OFF
- Commit al final de proceso exitoso
- Rollback en caso de error
- Log de todas las operaciones

### Recuperación de Errores
- Reintentos en bloqueos
- Timeout configurable
- Fallback a archivo COBOL si RDB falla
- Modo dual (archivo + RDB) disponible

## Características Especiales

### Modo Dual
- Escribe en archivo COBOL y RDB simultáneamente
- Lee de archivo, sobrescribe con RDB si existe
- Útil para migración gradual

### Conversión de Datos
- Mapeo automático COBOL ↔ SQL
- Manejo de COMP-3 y formatos packed
- Conversión de fechas y timestamps
- Preservación de precisión decimal

### Optimización
- Cursores para operaciones masivas
- Prepared statements reutilizables
- Batch inserts cuando es posible
- Índices en claves frecuentes

### Logging y Auditoría
- Registro completo de operaciones
- Timestamps en cada acción
- Contadores de registros procesados
- Errores detallados con contexto

## Configuración

### Parámetros en acas.param
```
HOST=localhost
USER=acas_user
PASSWORD=encrypted_pass
DATABASE=acas_db
PORT=3306
SOCKET=/var/mysql/mysql.sock
```

### Variables de Entorno
- ACAS_BIN: Directorio de ejecutables
- ACAS_LEDGERS: Directorio de datos
- COB_FILE_PATH: Rutas de archivos COBOL

## Notas de Implementación

### Consideraciones de Diseño
- Separación clara entre lógica de archivo y RDB
- Interfaz consistente para todos los archivos
- Reutilización de código mediante copybooks
- Manejo de errores centralizado

### Limitaciones
- Tamaño máximo de registro según COBOL
- Límites de MySQL para campos y tablas
- Sin soporte para BLOBs grandes
- Transacciones limitadas a una sesión

### Mejores Prácticas
- Siempre verificar FS-Reply después de operaciones
- Usar START antes de READ-NEXT para posicionar
- Cerrar explícitamente archivos y conexiones
- Monitorear logs para detectar problemas

### Mantenimiento
- Backup regular de archivos .seq
- Índices de base de datos optimizados
- Purga periódica de logs antiguos
- Verificación de integridad programada