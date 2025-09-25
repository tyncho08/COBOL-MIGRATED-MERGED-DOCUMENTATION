# MDM - Master Data Management Subsystem

## Especificaciones

### Propósito
El subsistema MDM (Master Data Management) proporciona gestión centralizada de datos maestros críticos para el sistema ACAS. Incluye la administración del Chart of Accounts (COA), configuración del sistema, parámetros de valores y datos de análisis que forman la base para todas las operaciones contables y financieras.

### Componentes Principales

1. **Nominal (Chart of Accounts)**
   - nominalMT.cbl - Manejador de tabla nominal
   - nominalLD.cbl - Cargador de datos nominal
   - nominalUNL.cbl - Descargador de datos nominal
   - nominalRES.cbl - Restaurador de datos nominal

2. **System Configuration**
   - systemMT.cbl - Manejador de configuración del sistema
   - systemLD.cbl - Cargador de configuración
   - systemUNL.cbl - Descargador de configuración
   - systemRES.cbl - Restaurador de configuración

3. **Value Parameters**
   - valueMT.cbl - Manejador de parámetros de valores
   - valueLD.cbl - Cargador de parámetros
   - valueUNL.cbl - Descargador de parámetros
   - valueRES.cbl - Restaurador de parámetros

4. **Analysis Data**
   - analMT.cbl - Manejador de datos de análisis
   - analLD.cbl - Cargador de análisis
   - analUNL.cbl - Descargador de análisis
   - analRES.cbl - Restaurador de análisis

## Interfaces

### Estructura de Datos Nominal (COA)
```cobol
01 NOMINAL-REC.
   03 NOMINAL-KEY.
      05 Company-Number      PIC 99.
      05 Account-Number      PIC 9(6).
   03 Account-Name           PIC X(40).
   03 Account-Type           PIC X.     *> A/L/E/I/C
   03 Account-Category       PIC XX.
   03 Balance-Type           PIC X.     *> D/C
   03 Department-Code        PIC XX.
   03 Analysis-Code          PIC X(4).
   03 VAT-Code              PIC X.
   03 Budget-Amount          PIC S9(11)V99.
   03 YTD-Balance           PIC S9(11)V99.
   03 Last-Year-Balance     PIC S9(11)V99.
   03 Account-Status        PIC X.     *> A/I/C
   03 Date-Created          PIC 9(8).
   03 Date-Modified         PIC 9(8).
   03 Created-By            PIC X(8).
   03 Modified-By           PIC X(8).
```

### Estructura de Datos del Sistema
```cobol
01 SYSTEM-REC.
   03 System-Key            PIC 99.
   03 Company-Name          PIC X(40).
   03 Company-Address-1     PIC X(40).
   03 Company-Address-2     PIC X(40).
   03 Company-City          PIC X(25).
   03 Company-State         PIC XX.
   03 Company-Zip           PIC X(10).
   03 Company-Phone         PIC X(15).
   03 Tax-ID                PIC X(15).
   03 Fiscal-Year-End       PIC 9(4).
   03 Current-Period        PIC 99.
   03 GL-Module-Active      PIC X.     *> Y/N
   03 AP-Module-Active      PIC X.
   03 AR-Module-Active      PIC X.
   03 INV-Module-Active     PIC X.
   03 PR-Module-Active      PIC X.
   03 RDBMS-DB-Name         PIC X(20).
   03 RDBMS-Host            PIC X(30).
   03 RDBMS-User            PIC X(20).
   03 RDBMS-Password        PIC X(20).
   03 RDBMS-Port            PIC X(6).
   03 RDBMS-Socket          PIC X(30).
   03 FS-Cobol-Files-Used   PIC X.     *> Y/N
   03 FS-RDB-Tables-Used    PIC X.
   03 FS-Dual-Mode-Active   PIC X.
```

### Estructura de Parámetros de Valores
```cobol
01 VALUE-REC.
   03 Value-Key             PIC X(10).
   03 Value-Description     PIC X(30).
   03 Value-Type            PIC X.     *> N/A/D/T
   03 Numeric-Value         PIC S9(11)V99.
   03 Alpha-Value           PIC X(50).
   03 Date-Value            PIC 9(8).
   03 Time-Value            PIC 9(6).
   03 Default-Value         PIC X(50).
   03 Valid-Range-From      PIC X(50).
   03 Valid-Range-To        PIC X(50).
   03 Editable-Flag         PIC X.     *> Y/N
   03 System-Flag           PIC X.     *> Y/N (protegido)
   03 Module-Owner          PIC XX.
```

## Flujos de Proceso

### Flujo de Gestión del Chart of Accounts
1. **Creación de Cuenta**:
   - Validar número de cuenta único
   - Verificar tipo y categoría
   - Asignar códigos de análisis
   - Establecer balances iniciales
   - Registrar metadatos de auditoría

2. **Modificación de Cuenta**:
   - Verificar permisos de edición
   - Validar integridad referencial
   - Actualizar campos permitidos
   - Mantener histórico de cambios
   - Actualizar timestamp y usuario

3. **Eliminación de Cuenta**:
   - Verificar uso en transacciones
   - Marcar como inactiva si tiene movimientos
   - Eliminación física solo si sin uso
   - Registrar acción de auditoría

### Flujo de Configuración del Sistema
1. **Inicialización**:
   - Crear registro de sistema base
   - Configurar módulos activos
   - Establecer parámetros contables
   - Definir conexiones RDB

2. **Actualización de Configuración**:
   - Validar cambios de configuración
   - Verificar impacto en módulos activos
   - Aplicar cambios de forma transaccional
   - Notificar cambios a subsistemas

### Flujo de Gestión de Parámetros
1. **Definición de Parámetros**:
   - Crear clave única de parámetro
   - Definir tipo y rango válido
   - Establecer valor por defecto
   - Asignar permisos de edición

2. **Consulta de Parámetros**:
   - Búsqueda por clave o descripción
   - Aplicar filtros por módulo
   - Validar permisos de acceso
   - Retornar valor formateado

## Dependencias

### Módulos de Acceso a Datos
- acas000: Manejador de archivos system/default/final
- acas005: Manejador de archivo nominal
- acas011: Manejador de archivo value
- acas015: Manejador de archivo analysis

### Archivos Maestros
- system.dat: Configuración principal del sistema
- nominal.dat: Chart of accounts
- value.dat: Parámetros del sistema
- analysis.dat: Códigos de análisis

### Dependencias RDB
- MySQL client libraries
- DAL específicos por RDBMS
- mysql-procedures.cpy
- mysql-variables.cpy

## Manejo de Errores

### Códigos de Error MDM
- MD001: Cuenta duplicada en COA
- MD002: Tipo de cuenta inválido
- MD003: Error en validación de balance
- MD004: Referencia circular en COA
- MD005: Parámetro del sistema no encontrado
- MD006: Valor fuera de rango válido
- MD007: Configuración de módulo inconsistente
- MD008: Error de integridad referencial

### Validaciones de Integridad
- Cuentas padre-hijo en COA
- Consistencia entre módulos activos
- Rangos válidos para parámetros numéricos
- Formatos de fecha y tiempo
- Valores requeridos vs opcionales

## Consideraciones de Seguridad

### Control de Acceso
- Permisos diferenciados por tipo de dato maestro
- Protección especial para configuración del sistema
- Auditoría completa de cambios
- Restricciones de eliminación

### Integridad de Datos
- Validaciones de negocio en escritura
- Verificaciones de consistencia
- Backup automático antes de cambios críticos
- Rollback en caso de error

### Configuración Sensible
- Credenciales RDB encriptadas
- Parámetros del sistema protegidos
- Configuración de módulos validada
- Acceso restringido a datos de conexión

## Características Especiales

### Gestión Jerárquica del COA
- Soporte para cuentas padre-hijo
- Navegación por niveles
- Consolidación automática
- Validación de estructura

### Configuración Dinámica
- Cambios de configuración sin reinicio
- Notificación automática a módulos
- Validación de dependencias
- Rollback de configuración

### Parámetros Tipados
- Soporte para múltiples tipos de datos
- Validación de rango automática
- Conversión de tipos transparente
- Valores por defecto inteligentes

## Notas de Implementación

### Consideraciones de Rendimiento
- Índices optimizados para consultas frecuentes
- Cache de parámetros del sistema
- Lazy loading para datos poco usados
- Batch updates para cambios masivos

### Mantenimiento
- Limpieza periódica de datos obsoletos
- Optimización de índices
- Backup incremental de datos maestros
- Verificación de integridad programada

### Migración de Datos
- Herramientas de importación masiva
- Validación de datos migrados
- Mapeo de sistemas legacy
- Preservación de relaciones

### Limitaciones
- Máximo 999,999 cuentas por compañía
- Longitud limitada de nombres y descripciones
- Sin soporte para Unicode en versión actual
- Relaciones jerárquicas limitadas a 10 niveles