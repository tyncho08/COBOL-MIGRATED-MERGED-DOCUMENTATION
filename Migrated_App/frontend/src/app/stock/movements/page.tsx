'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import {
  CubeIcon,
  ArrowDownIcon,
  ArrowUpIcon,
  ArrowsRightLeftIcon,
  DocumentTextIcon,
  FunnelIcon,
  ArrowDownTrayIcon,
  PrinterIcon,
  CalendarIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'
import Table from '@/components/UI/Table'

interface StockMovement {
  id: number
  item_code: string
  description: string
  movement_type: string
  quantity: number
  location: string
  reference: string
  date: string
}

export default function StockMovementsPage() {
  const router = useRouter()
  const [movements, setMovements] = useState<StockMovement[]>([])
  const [loading, setLoading] = useState(true)
  const [filters, setFilters] = useState({
    dateFrom: '',
    dateTo: '',
    movementType: '',
    itemCode: '',
    location: ''
  })
  const [showFilters, setShowFilters] = useState(false)

  useEffect(() => {
    fetchMovements()
  }, [filters])

  const fetchMovements = async () => {
    setLoading(true)
    try {
      const queryParams = new URLSearchParams()
      Object.entries(filters).forEach(([key, value]) => {
        if (value) queryParams.append(key, value)
      })
      
      const response = await fetch(`http://localhost:8000/api/v1/stock/recent-movements`)
      if (response.ok) {
        const data = await response.json()
        setMovements(data || [])
      } else {
        console.error('Failed to fetch movements:', response.status)
        setMovements([])
      }
    } catch (error) {
      console.error('Failed to fetch movements:', error)
      setMovements([])
    } finally {
      setLoading(false)
    }
  }


  const getMovementTypeColor = (type: string) => {
    switch (type) {
      case 'RECEIPT':
        return 'bg-green-100 text-green-700'
      case 'ISSUE':
        return 'bg-red-100 text-red-700'
      case 'ADJUSTMENT':
        return 'bg-yellow-100 text-yellow-700'
      case 'TRANSFER':
        return 'bg-blue-100 text-blue-700'
      case 'RETURN':
        return 'bg-purple-100 text-purple-700'
      default:
        return 'bg-gray-100 text-gray-700'
    }
  }

  const getMovementIcon = (type: string) => {
    switch (type) {
      case 'RECEIPT':
        return <ArrowDownIcon className="h-4 w-4" />
      case 'ISSUE':
        return <ArrowUpIcon className="h-4 w-4" />
      case 'ADJUSTMENT':
        return <DocumentTextIcon className="h-4 w-4" />
      case 'TRANSFER':
        return <ArrowsRightLeftIcon className="h-4 w-4" />
      default:
        return <CubeIcon className="h-4 w-4" />
    }
  }

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(value)
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    })
  }

  const columns = [
    {
      key: 'movement_type',
      header: 'Type',
      render: (_: any, movement: StockMovement) => (
        <div className="flex items-center gap-2">
          <div className={`p-1 rounded ${getMovementTypeColor(movement.movement_type)}`}>
            {getMovementIcon(movement.movement_type)}
          </div>
          <span className={`text-xs font-medium px-2 py-1 rounded-full ${getMovementTypeColor(movement.movement_type)}`}>
            {movement.movement_type}
          </span>
        </div>
      )
    },
    {
      key: 'date',
      header: 'Date',
      render: (_: any, movement: StockMovement) => (
        <span className="text-sm text-gray-600">{formatDate(movement.date)}</span>
      )
    },
    {
      key: 'item',
      header: 'Item',
      render: (_: any, movement: StockMovement) => (
        <div>
          <p className="font-medium text-gray-900">{movement.item_code}</p>
          <p className="text-sm text-gray-500">{movement.description}</p>
        </div>
      )
    },
    {
      key: 'location',
      header: 'Location',
      render: (_: any, movement: StockMovement) => (
        <div className="text-sm">
          <p>{movement.location}</p>
        </div>
      )
    },
    {
      key: 'quantity',
      header: 'Quantity',
      render: (_: any, movement: StockMovement) => (
        <span className={`font-medium ${movement.quantity >= 0 ? 'text-green-600' : 'text-red-600'}`}>
          {movement.quantity >= 0 ? '+' : ''}{movement.quantity}
        </span>
      )
    },
    {
      key: 'reference',
      header: 'Reference',
      render: (_: any, movement: StockMovement) => (
        <div>
          <p className="text-sm font-medium text-gray-900">{movement.reference}</p>
        </div>
      )
    }
  ]

  const handleExport = (format: 'csv' | 'excel' | 'pdf') => {
    console.log(`Exporting movements as ${format}`)
    alert(`Export to ${format.toUpperCase()} functionality would be implemented here`)
  }

  const clearFilters = () => {
    setFilters({
      dateFrom: '',
      dateTo: '',
      movementType: '',
      itemCode: '',
      location: ''
    })
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Stock Movements"
        description="View all stock movement history"
        actions={
          <div className="flex items-center gap-3">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowFilters(!showFilters)}
              icon={<FunnelIcon className="h-4 w-4" />}
            >
              Filters
              {Object.values(filters).some(v => v) && (
                <span className="ml-2 h-2 w-2 bg-indigo-600 rounded-full"></span>
              )}
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleExport('excel')}
              icon={<ArrowDownTrayIcon className="h-4 w-4" />}
            >
              Export
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => window.print()}
              icon={<PrinterIcon className="h-4 w-4" />}
            >
              Print
            </Button>
          </div>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Stock Control', href: '/stock' },
          { label: 'Movements' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Filters */}
        {showFilters && (
          <Card className="mb-6">
            <div className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-5 gap-4">
                <Input
                  label="Date From"
                  type="date"
                  value={filters.dateFrom}
                  onChange={(e) => setFilters({ ...filters, dateFrom: e.target.value })}
                />
                <Input
                  label="Date To"
                  type="date"
                  value={filters.dateTo}
                  onChange={(e) => setFilters({ ...filters, dateTo: e.target.value })}
                />
                <Select
                  label="Movement Type"
                  value={filters.movementType}
                  onChange={(e) => setFilters({ ...filters, movementType: e.target.value })}
                >
                  <option value="">All Types</option>
                  <option value="RECEIPT">Receipt</option>
                  <option value="ISSUE">Issue</option>
                  <option value="ADJUSTMENT">Adjustment</option>
                  <option value="TRANSFER">Transfer</option>
                  <option value="RETURN">Return</option>
                </Select>
                <Input
                  label="Item Code"
                  type="text"
                  value={filters.itemCode}
                  onChange={(e) => setFilters({ ...filters, itemCode: e.target.value })}
                  placeholder="Search item code..."
                />
                <Input
                  label="Location"
                  type="text"
                  value={filters.location}
                  onChange={(e) => setFilters({ ...filters, location: e.target.value })}
                  placeholder="Search location..."
                />
              </div>
              <div className="mt-4 flex justify-end">
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={clearFilters}
                >
                  Clear Filters
                </Button>
              </div>
            </div>
          </Card>
        )}

        {/* Movements Summary */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
          <Card className="p-4">
            <div className="flex items-center">
              <ArrowDownIcon className="h-8 w-8 text-green-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Total Receipts</p>
                <p className="text-xl font-bold text-gray-900">
                  {movements.filter(m => m.movement_type === 'RECEIPT').length}
                </p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <ArrowUpIcon className="h-8 w-8 text-red-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Total Issues</p>
                <p className="text-xl font-bold text-gray-900">
                  {movements.filter(m => m.movement_type === 'ISSUE').length}
                </p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <DocumentTextIcon className="h-8 w-8 text-yellow-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Adjustments</p>
                <p className="text-xl font-bold text-gray-900">
                  {movements.filter(m => m.movement_type === 'ADJUSTMENT').length}
                </p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <ArrowsRightLeftIcon className="h-8 w-8 text-blue-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Transfers</p>
                <p className="text-xl font-bold text-gray-900">
                  {movements.filter(m => m.movement_type === 'TRANSFER').length}
                </p>
              </div>
            </div>
          </Card>
        </div>

        {/* Movements Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Movement History</h3>
          </div>
          <Table
            data={movements}
            columns={columns}
            loading={loading}
            emptyMessage="No stock movements found"
          />
        </Card>
      </main>
    </div>
  )
}