# Documentación de Scripts del Sistema ACAS Legacy

## Índice
1. [Scripts de Instalación](#scripts-de-instalación)
2. [Scripts de Compilación General](#scripts-de-compilación-general)
3. [Scripts de Compilación por Módulo](#scripts-de-compilación-por-módulo)
4. [Scripts de Backup](#scripts-de-backup)
5. [Scripts de Utilidad](#scripts-de-utilidad)
6. [Patrones y Convenciones](#patrones-y-convenciones)
7. [Guía Operacional](#guía-operacional)

---

## Scripts de Instalación

### install-ACAS.sh
**Propósito**: Script principal de instalación inicial del sistema ACAS para nuevos usuarios.

**Parámetros**: Ninguno (debe ejecutarse desde el directorio ACAS)

**Dependencias**:
- Directorio fuente con todos los módulos COBOL compilados
- Estructura de directorios: common/, irs/, sales/, purchase/, stock/, general/

**Proceso que ejecuta**:
1. Crea directorio ~/bin si no existe
2. Establece permisos de ejecución en todos los scripts (.sh)
3. Copia scripts de backup (acasbkup*.sh) a ~/bin
4. Procesa módulo common: copia masterLD.sh, binarios RDBMS/No-RDBMS
5. Procesa cada subsistema (irs, sales, purchase, stock, general)
6. Configura variables de entorno en ~/.bashrc
7. Crea estructura de directorios de datos (~/ACAS, ~/ACAS-practice)

**Archivos que genera**:
- ~/bin/: Todos los ejecutables y librerías (.so)
- ~/.bashrc: Variables de entorno ACAS
- ~/ACAS/: Directorio principal de datos
- ~/ACAS/temp-backups/: Backups temporales
- ~/ACAS/archives/: Archivos archivados
- ~/ACAS-practice/: Entorno de pruebas

**Variables de entorno configuradas**:
```bash
COB_SCREEN_ESC=YES
COB_SCREEN_EXCEPTIONS=YES
COB_LIBRARY_PATH=~/bin
COB_EXIT_WAIT=on
PATH=~/bin:.:$PATH
ACAS_LEDGERS=~/ACAS
ACAS_BIN=~/bin
TMPDIR=~/tmp
```

### install-ACAS-preinstalled.sh
**Propósito**: Reinstalación de programas compilados cuando la instalación inicial ya se ejecutó previamente.

**Diferencias con install-ACAS.sh**:
- No modifica ~/.bashrc
- Solo actualiza binarios y librerías
- Más rápido para actualizaciones

**Uso recomendado**: Después de recompilaciones o actualizaciones del sistema.

---

## Scripts de Compilación General

### comp-all.sh
**Propósito**: Compilación completa del sistema con soporte RDBMS (MySQL).

**Dependencias**:
- GnuCOBOL (cobc)
- MySQL client libraries
- Directorio copybooks/ con archivos de copia

**Proceso**:
1. Configura COBCPY=../copybooks
2. Compila en orden: common → general → irs → purchase → sales → stock
3. Cada módulo se compila con su script específico comp-*.sh

**Variables de entorno requeridas**:
- COBCPY: Ruta a copybooks
- COB_COPY_DIR: Ruta a copybooks

### comp-all-no-rdbms.sh
**Propósito**: Compilación completa sin soporte RDBMS (archivos planos únicamente).

**Diferencias**:
- Llama scripts comp-*-no-rdbms.sh
- No requiere librerías MySQL
- Usa dummy-rdbmsMT.cbl como stub

### comp-all-diags.sh
**Propósito**: Compilación con diagnósticos habilitados para debugging.

**Características**:
- Activa opciones de diagnóstico del compilador
- Genera información adicional de debugging
- Útil para resolución de problemas

---

## Scripts de Compilación por Módulo

### Módulo Common (common/)

#### comp-common.sh (Con RDBMS)
**Propósito**: Compilación del módulo común con soporte MySQL.

**Proceso detallado**:
1. **accept_numeric.c**: Compilación de rutinas numéricas
2. **dummy-rdbmsMT.cbl**: Stubs RDBMS de respaldo
3. **ACAS-Sysout.cbl**: Manejo de salida del sistema
4. **Archivos *MT.scb**: Procesamiento SQL con presql2
5. **Archivos *MT.cbl**: Módulos transaccionales con MySQL
6. **maps0*.cbl**: Módulos de mapeo
7. **acas0*.cbl**: Módulos core del sistema
8. **acasirsub*.cbl**: Subrutinas IRS
9. **Utilidades**: fhlogger, xl150, sys002
10. **ACAS.cbl**: Programa principal
11. **Archivos *LD.cbl**: Carga de datos
12. **Archivos *UNL.cbl**: Descarga de datos
13. **Archivos *RES.cbl**: Restauración de datos

**Opciones de compilación**:
```bash
cobc -m $archivo cobmysqlapi.o -I ../copybooks -Wlinkage \
     -L/usr/local/mysql/lib -lmysqlclient -lz \
     -fdump=all -fmissing-statement=ok
```

#### comp-common-no-rdbms.sh
**Diferencias**: Sin librerías MySQL, usa dummy-rdbmsMT.cbl para todos los módulos.

### Módulo Sales (sales/)

#### comp-sales.sh
**Programas compilados**:
- sl*.cbl: Módulos de ventas
- sales.cbl: Programa principal de ventas (ejecutable y módulo)

**Nota**: Versión actual (29/07/2025) removió accept_numeric por problemas de compatibilidad.

### Módulos por Subsistema

Cada subsistema (irs/, purchase/, stock/, general/) sigue el patrón:
- **comp-{modulo}.sh**: Versión con RDBMS
- **comp-{modulo}-no-rdbms.sh**: Versión sin RDBMS  
- **comp-{modulo}-diags.sh**: Versión con diagnósticos

---

## Scripts de Backup

### acasbkup.sh
**Propósito**: Backup estándar de datos ACAS.

**Proceso**:
1. Crea directorio temp-backups si no existe
2. Genera archivo tar.gz con timestamp: `acas-bkup-YYYYMMDDHHMMS.tar.gz`
3. Incluye todos los archivos .dat

**Formato de archivo**: `acas-bkup-20241225143022.tar.gz`

### acasbkup-Pre-EOY.sh
**Propósito**: Backup específico antes de procesos de fin de año (irs060, XL150).

**Diferencias**:
- Nombre incluye sufijo "Pre-EOY"
- Se ejecuta automáticamente antes de procesos críticos de fin de año

### acasbkup-Post-EOY.sh  
**Propósito**: Backup después de procesos de fin de año.

**Uso**: Respaldo posterior a la ejecución exitosa de irs060 o XL150.

**Ubicación de backups**: `temp-backups/acas-bkup-TIMESTAMP-Post-EOY.tar.gz`

---

## Scripts de Utilidad

### masterLD.sh
**Propósito**: Carga masiva de datos desde archivos COBOL a tablas RDBMS.

**Dependencias**:
- Sistema configurado para RDBMS
- Parámetros ACAS correctamente configurados
- Archivos de datos en ~/ACAS/

**Proceso**:
1. **system.dat**: Procesa 5 tipos de registros diferentes
   - systemLD, sys4LD, finalLD, dfltLD
2. **Archivos de datos**: Verifica existencia y carga cada tabla
   - analysis.dat → analLD
   - batch.dat → glbatchLD  
   - ledger.dat → nominalLD
   - stockctl.dat → stockLD
   - Etc. (20+ tipos de archivos)

**Códigos de retorno**:
- 0: Éxito
- 16: Error escribiendo a RDBMS
- 64: RDBMS no configurado
- 128: Parámetros no configurados

### prtpdf.sh
**Propósito**: Conversión de reportes a PDF.

**Uso**: `prtpdf filename` (sin extensión)

**Proceso**:
1. Usa enscript para formatear texto
2. Convierte a PDF con ps2pdf14
3. Configuración específica para facturas (sl930-Email)

**Dependencias**:
- enscript
- ghostscript-common

**Configuración**:
- Formato A4
- Fuente Courier9@9/8
- Landscape
- Márgenes: 30:30:10:10

### chgfmt-all.sh (stock/)
**Propósito**: Cambio de formato de archivos del módulo stock.

**Proceso**: Convierte archivos st0*.cbl a formato fijo usando changeformat.

### dykegrove.sh
**Propósito**: Script específico del cliente Dykegrove.

**Función**: 
- Cambia al directorio de datos del cliente
- Configura ACAS_LEDGERS específico
- Ejecuta sistema ACAS

---

## Patrones y Convenciones

### Estructura de Nombres
- **comp-{modulo}.sh**: Compilación estándar con RDBMS
- **comp-{modulo}-no-rdbms.sh**: Compilación sin RDBMS  
- **comp-{modulo}-diags.sh**: Compilación con diagnósticos
- **acasbkup*.sh**: Scripts de backup con variantes

### Convenciones de Compilación

#### Opciones Estándar con RDBMS:
```bash
cobc -m programa.cbl cobmysqlapi.o \
     -I ../copybooks \
     -Wlinkage \
     -L/usr/local/mysql/lib \
     -lmysqlclient -lz \
     -fdump=all \
     -fmissing-statement=ok
```

#### Opciones Sin RDBMS:
```bash
cobc -m programa.cbl dummy-rdbmsMT.cbl \
     -I ../copybooks
```

### Estructura de Directorios
```
Legacy_App/
├── common/           # Módulos compartidos
├── general/          # Contabilidad general  
├── irs/             # Sistema IRS
├── purchase/        # Compras
├── sales/           # Ventas
├── stock/           # Control de stock
├── copybooks/       # Archivos de copia COBOL
├── install-*.sh     # Scripts de instalación
├── comp-all*.sh     # Compilación completa
└── acasbkup*.sh     # Scripts de backup
```

### Variables de Entorno Estándar
- **COBCPY**: Ruta a copybooks para compilador
- **COB_COPY_DIR**: Directorio de archivos de copia
- **ACAS_LEDGERS**: Directorio de datos ACAS
- **ACAS_BIN**: Directorio de ejecutables
- **COB_LIBRARY_PATH**: Ruta de librerías COBOL

---

## Guía Operacional para Administradores

### Instalación Inicial

1. **Primera instalación**:
   ```bash
   cd /path/to/Legacy_App
   ./install-ACAS.sh
   ```

2. **Reiniciar terminal** para cargar nuevas variables de entorno

3. **Verificar instalación**:
   ```bash
   ls ~/bin/
   echo $ACAS_LEDGERS
   ```

### Compilación del Sistema

1. **Compilación completa con RDBMS**:
   ```bash
   ./comp-all.sh
   ```

2. **Compilación sin RDBMS**:
   ```bash
   ./comp-all-no-rdbms.sh
   ```

3. **Compilación con diagnósticos**:
   ```bash
   ./comp-all-diags.sh
   ```

4. **Reinstalación después de compilar**:
   ```bash
   ./install-ACAS-preinstalled.sh
   ```

### Operaciones de Backup

1. **Backup manual**:
   ```bash
   cd ~/ACAS
   ~/bin/acasbkup.sh
   ```

2. **Backup pre fin de año**:
   ```bash
   ~/bin/acasbkup-Pre-EOY.sh
   ```

3. **Verificar backups**:
   ```bash
   ls -la ~/ACAS/temp-backups/
   ```

### Mantenimiento de Base de Datos (RDBMS)

1. **Carga inicial de datos**:
   ```bash
   cd ~/ACAS
   ~/bin/masterLD.sh
   ```

2. **Verificar carga**:
   ```bash
   less SYS-DISPLAY.log
   ```

### Resolución de Problemas

#### Problemas de Compilación
1. **Verificar dependencias**:
   - GnuCOBOL instalado
   - Librerías MySQL (si RDBMS)
   - Permisos de archivos

2. **Compilación con diagnósticos**:
   ```bash
   ./comp-all-diags.sh
   ```

3. **Verificar copybooks**:
   ```bash
   ls -la copybooks/
   echo $COBCPY
   ```

#### Problemas de Ejecución
1. **Verificar variables de entorno**:
   ```bash
   env | grep COB
   env | grep ACAS
   ```

2. **Verificar permisos**:
   ```bash
   ls -la ~/bin/
   ```

3. **Verificar estructura de datos**:
   ```bash
   ls -la ~/ACAS/
   ```

### Tareas de Mantenimiento Regulares

#### Diario
- Verificar logs del sistema
- Monitorear espacio en ~/ACAS/temp-backups/

#### Semanal  
- Limpiar backups antiguos en temp-backups/
- Verificar integridad de datos

#### Mensual
- Backup completo a almacenamiento externo
- Verificar actualizaciones de sistema

#### Fin de Año
1. Ejecutar acasbkup-Pre-EOY.sh
2. Procesar IRS (irs060) o XL150
3. Ejecutar acasbkup-Post-EOY.sh
4. Verificar integridad de datos post-proceso

### Scripts de Cliente Específico

Para configuraciones específicas como Dykegrove:
1. Crear script similar a dykegrove.sh
2. Configurar ACAS_LEDGERS específico
3. Establecer directorio de datos del cliente

### Monitoreo y Logs

**Archivos de log importantes**:
- `SYS-DISPLAY.log`: Salida del sistema ACAS
- `temp-backups/`: Archivos de respaldo
- Logs de compilación en cada directorio de módulo

**Verificación de estado**:
```bash
# Verificar procesos ACAS
ps aux | grep -i acas

# Verificar espacio en disco
df -h ~/ACAS

# Verificar últimos backups
ls -lt ~/ACAS/temp-backups/ | head -5
```

---

## Notas Importantes

1. **Compatibilidad**: Los scripts están diseñados para GnuCOBOL y sistemas Unix/Linux.

2. **Permisos**: Todos los scripts requieren permisos de ejecución (chmod +x).

3. **Rutas**: Los scripts asumen estructura específica de directorios.

4. **RDBMS vs No-RDBMS**: Mantener consistencia en la elección de modo de compilación.

5. **Backups**: Los nombres de archivo acasbkup*.sh están hardcodeados en menús ACAS.

6. **Actualizaciones**: Usar install-ACAS-preinstalled.sh para actualizaciones, no install-ACAS.sh.

---

*Documentación generada para el sistema ACAS Legacy - Versión 1.0*
*Fecha: 2024*