# ERROR_FW - Error Framework Subsystem

## Especificaciones

### Propósito
El subsistema ERROR_FW proporciona un framework centralizado para el manejo, logging y reporte de errores en todo el sistema ACAS. Gestiona códigos de error estandarizados, mensajes informativos, logging de eventos y mecanismos de recuperación automática.

### Componentes Principales

1. **Sistema Central de Errores**
   - Códigos de error estandarizados por módulo
   - Mensajes de error configurables
   - Niveles de severidad (Info, Warning, Error, Fatal)
   - Contexto de error con stack trace

2. **Logging Framework**
   - fhlogger.cbl - Logger principal del sistema
   - ACAS-Sysout.cbl - Módulo de salida estándar
   - Rotación automática de logs
   - Múltiples niveles de logging

3. **Manejo de Estados**
   - FS-Reply - Códigos de retorno de archivos
   - WE-Error - Códigos de error específicos del sistema
   - SQL-State - Estados SQL estándar ANSI
   - Return-Code - Códigos de salida del sistema

4. **Recuperación y Continuidad**
   - Mecanismos de rollback automático
   - Reintentos configurables
   - Fallback a sistemas alternativos
   - Notificaciones de errores críticos

## Interfaces

### Estructura de Error Estándar
```cobol
01 ERROR-CONTEXT.
   03 Error-Code           PIC X(5).
   03 Error-Module         PIC X(15).
   03 Error-Program        PIC X(15).
   03 Error-Line-Number    PIC 9(5).
   03 Error-Severity       PIC X.      *> I/W/E/F
   03 Error-Timestamp      PIC X(26).
   03 Error-Message        PIC X(80).
   03 Error-Details        PIC X(255).
   03 User-ID              PIC X(8).
   03 Terminal-ID          PIC X(8).
   03 Transaction-ID       PIC X(10).
   03 Recovery-Action      PIC XX.     *> RR/AB/CO/IG
```

### Códigos de Error por Subsistema
```cobol
*> System-wide errors (SY001-SY999)
01 SYSTEM-ERRORS.
   03 SY001    PIC X(26) VALUE "SY001 System file read error".
   03 SY004    PIC X(59) VALUE "SY004 Problem opening system file".
   03 SY006    PIC X(62) VALUE "SY006 Program arguments limited".
   03 SY007    PIC X(35) VALUE "SY007 Program arguments incorrect".
   03 SY008    PIC X(31) VALUE "SY008 Note message & Hit return".
   03 SY009    PIC X(53) VALUE "SY009 Environment variables not set".
   03 SY019    PIC X(38) VALUE "SY019 DB not configured in param".

*> General Ledger errors (GL001-GL999)
01 GL-ERRORS.
   03 GL010    PIC X(30) VALUE "GL010 Duplicate keys found".
   03 GL011    PIC X(39) VALUE "GL011 Error on processing".
   03 GL012    PIC X(36) VALUE "GL012 Error opening file".
   03 GL013    PIC X(30) VALUE "GL013 Error reading file".
   03 GL014    PIC X(31) VALUE "GL014 Error writing table".

*> Database/RDB errors (SM001-SM999)
01 RDB-ERRORS.
   03 SM004    PIC X(30) VALUE "SM004 SQL Error in procedures".
   03 SM901    PIC X(31) VALUE "SM901 Note error and hit return".
```

### Estados de Respuesta del Sistema
```cobol
01 RESPONSE-CODES.
   03 SUCCESS-CODES.
      05 RC-SUCCESS        VALUE 0.
      05 RC-EOF           VALUE 10.
   03 WARNING-CODES.
      05 RC-DUPLICATE     VALUE 22.
      05 RC-NOT-FOUND     VALUE 23.
   03 ERROR-CODES.
      05 RC-ACCESS-DENIED VALUE 21.
      05 RC-FILE-ERROR    VALUE 35.
      05 RC-GENERIC-ERROR VALUE 99.
   03 FATAL-CODES.
      05 RC-SYSTEM-ERROR  VALUE 128.
      05 RC-CONFIG-ERROR  VALUE 64.
      05 RC-RDB-ERROR     VALUE 16.
```

## Flujos de Proceso

### Flujo de Manejo de Errores
1. **Detección de Error**:
   - Captura automática de excepciones
   - Verificación de códigos de retorno
   - Validación de estados SQL
   - Detección de condiciones anómalas

2. **Clasificación y Contexto**:
   - Determinar severidad del error
   - Capturar contexto de ejecución
   - Identificar módulo y programa origen
   - Asignar código de error estándar

3. **Logging y Registro**:
   - Escribir entrada detallada en log
   - Incluir timestamp y usuario
   - Registrar stack trace si aplica
   - Actualizar contadores de error

4. **Acción de Recuperación**:
   - Evaluar opciones de recuperación
   - Ejecutar rollback si es necesario
   - Intentar operación alternativa
   - Notificar a usuario/administrador

### Flujo de Logging
1. **Inicialización**:
   - Abrir archivo de log correspondiente
   - Verificar permisos de escritura
   - Configurar nivel de logging
   - Inicializar buffers

2. **Escritura de Log**:
   - Formatear entrada de log
   - Incluir metadata requerida
   - Escribir a archivo/syslog
   - Flush buffer si es crítico

3. **Rotación de Logs**:
   - Verificar tamaño de archivo
   - Crear nuevo archivo si es necesario
   - Comprimir archivos antiguos
   - Mantener histórico configurable

### Flujo de Recuperación Automática
1. **Evaluación de Error**:
   - Analizar tipo y causa del error
   - Verificar si es recuperable
   - Consultar políticas de retry
   - Determinar estrategia óptima

2. **Ejecución de Recuperación**:
   - Limpiar estado inconsistente
   - Restaurar último punto válido
   - Ejecutar acción compensatoria
   - Reintentar operación original

3. **Verificación**:
   - Confirmar éxito de recuperación
   - Validar integridad de datos
   - Registrar acción tomada
   - Continuar procesamiento normal

## Dependencias

### Módulos del Sistema
- fhlogger: Logger principal
- ACAS-Sysout: Salida estándar
- acas007-acas015: Módulos de acceso a datos
- Sistema operativo: syslog, archivos de log

### Archivos de Configuración
- error-config.dat: Configuración de manejo de errores
- logging.conf: Configuración de logging
- recovery.params: Parámetros de recuperación

### Recursos Externos
- Directorio de logs del sistema
- Servicio de email para notificaciones
- Base de datos de eventos (opcional)
- Sistema de monitoreo externo

## Manejo de Errores del Framework

### Errores del Sistema de Logging
- LOG001: No se puede abrir archivo de log
- LOG002: Disk full - no se puede escribir log
- LOG003: Permisos insuficientes para logging
- LOG004: Error en rotación de logs

### Errores de Recuperación
- REC001: No se puede ejecutar rollback
- REC002: Punto de recovery no disponible
- REC003: Acción de recuperación falló
- REC004: Estado inconsistente después de recovery

### Errores de Configuración
- CFG001: Archivo de configuración no encontrado
- CFG002: Configuración inválida
- CFG003: Parámetros requeridos faltantes
- CFG004: Conflicto en configuración

## Características Especiales

### Logging Estructurado
- Formato JSON para logs complejos
- Campos estándar en todas las entradas
- Correlación de eventos relacionados
- Filtros y búsquedas avanzadas

### Notificaciones Inteligentes
- Escalamiento automático por severidad
- Agrupación de errores similares
- Supresión de spam de errores
- Integración con sistemas de alertas

### Métricas y Monitoreo
- Contadores de errores por tipo
- Tendencias de errores en tiempo
- Análisis de causa raíz automatizado
- Dashboards de salud del sistema

### Configuración Dinámica
- Cambio de nivel de logging sin reinicio
- Activación/desactivación de recovery
- Configuración de umbrales dinámicos
- Reglas de manejo personalizables

## Configuración del Framework

### Niveles de Logging
```
DEBUG   = Información detallada de desarrollo
INFO    = Eventos normales del sistema  
WARNING = Situaciones que requieren atención
ERROR   = Errores que no detienen el sistema
FATAL   = Errores que requieren parada del sistema
```

### Políticas de Recovery
```
RETRY_COUNT=3           # Número de reintentos
RETRY_DELAY=1000        # Delay entre reintentos (ms)
FALLBACK_ENABLED=Y      # Activar sistema fallback
ROLLBACK_AUTO=Y         # Rollback automático en errores
NOTIFY_ADMIN=Y          # Notificar administrador
```

### Configuración de Rotación
```
MAX_LOG_SIZE=100MB      # Tamaño máximo de archivo
LOG_RETENTION=30        # Días de retención
COMPRESS_OLD_LOGS=Y     # Comprimir logs antiguos
LOG_DIRECTORY=/acas/logs # Directorio de logs
```

## Notas de Implementación

### Consideraciones de Performance
- Logging asíncrono para operaciones críticas
- Buffers de memoria para reducir I/O
- Compresión de logs para ahorrar espacio
- Índices en logs para búsquedas rápidas

### Seguridad
- Logs protegidos contra modificación
- Sanitización de datos sensibles
- Control de acceso a logs
- Auditoría de acceso a logs

### Integración
- APIs estándar para otros módulos
- Compatibilidad con herramientas externas
- Export de métricas para monitoreo
- Webhooks para integraciones custom

### Limitaciones
- Overhead de performance en logging intensivo
- Espacio en disco requerido para logs históricos
- Complejidad adicional en debugging
- Dependencia de configuración correcta

### Mejores Prácticas
- Usar códigos de error estándar consistentemente
- Incluir contexto suficiente en mensajes
- Configurar alertas para errores críticos
- Revisar y limpiar logs periódicamente
- Documentar códigos de error nuevos
- Probar mecanismos de recovery regularmente